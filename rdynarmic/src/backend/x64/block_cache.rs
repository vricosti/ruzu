use crate::ir::location::LocationDescriptor;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

/// A compiled native code block.
pub struct CachedBlock {
    /// Absolute native code address (within the code buffer).
    pub entrypoint: *const u8,
    /// Offset from code buffer base.
    pub entrypoint_offset: usize,
    /// Size of the compiled native code in bytes.
    pub size: usize,
}

static NEXT_CACHE_ID: AtomicU64 = AtomicU64::new(0);

fn jit_stats_enabled() -> bool {
    use std::sync::OnceLock;
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_JIT_STATS").is_some())
}

fn jit_stats_interval() -> u64 {
    use std::sync::OnceLock;
    static INTERVAL: OnceLock<u64> = OnceLock::new();
    *INTERVAL.get_or_init(|| {
        std::env::var("RUZU_JIT_STATS_INTERVAL")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(250_000)
    })
}

/// Caller-callsite tag for `BlockCache::get_*` accounting.
#[derive(Debug, Clone, Copy)]
pub enum CacheCallSite {
    /// Read-only lookup (`lookup_cached_block`). Used by JIT-emitted
    /// fast paths that only consult the cache and don't compile on miss.
    Lookup,
    /// Compile-or-return path (`get_or_compile_block_with_ro`). One per
    /// JIT entry from the dispatcher.
    Compile,
    /// Block-linking dispatcher closure invoked from JIT-emitted code.
    Chain,
}

impl CacheCallSite {
    fn idx(self) -> usize {
        match self {
            CacheCallSite::Lookup => 0,
            CacheCallSite::Compile => 1,
            CacheCallSite::Chain => 2,
        }
    }
    fn label(self) -> &'static str {
        match self {
            CacheCallSite::Lookup => "lookup",
            CacheCallSite::Compile => "compile",
            CacheCallSite::Chain => "chain",
        }
    }
}

/// Cache of compiled blocks, keyed by LocationDescriptor (PC + FPCR hash).
///
/// Single-threaded: no internal locking (one JIT per CPU core).
pub struct BlockCache {
    id: u64,
    blocks: HashMap<LocationDescriptor, CachedBlock>,
    /// Total `get(...)` calls summed across all callsites.
    lookups: AtomicU64,
    /// Lookups that returned a cached block.
    hits: AtomicU64,
    /// Per-callsite counters (Lookup / Compile / Chain). `[lookups, hits]`.
    site_lookups: [AtomicU64; 3],
    site_hits: [AtomicU64; 3],
    /// `insert(...)` calls (one per fresh compile).
    inserts: AtomicU64,
    /// Distinct locations evicted by `invalidate_range`.
    invalidations: AtomicU64,
    /// `clear(...)` calls (full-cache wipes).
    clears: AtomicU64,
    /// Total nanoseconds spent inside `add_compile_time(ns)` — caller
    /// supplies elapsed time around the actual emit body.
    compile_time_ns: AtomicU64,
    /// Number of `add_compile_time` calls (== compile invocations).
    compile_count: AtomicU64,
}

impl BlockCache {
    pub fn new() -> Self {
        Self {
            id: NEXT_CACHE_ID.fetch_add(1, Ordering::Relaxed),
            blocks: HashMap::new(),
            lookups: AtomicU64::new(0),
            hits: AtomicU64::new(0),
            site_lookups: [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)],
            site_hits: [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)],
            inserts: AtomicU64::new(0),
            invalidations: AtomicU64::new(0),
            clears: AtomicU64::new(0),
            compile_time_ns: AtomicU64::new(0),
            compile_count: AtomicU64::new(0),
        }
    }

    /// Generic lookup (back-compat). Tags as `Chain` since the only
    /// remaining direct caller is the JIT-emitted fast-dispatch closure.
    pub fn get(&self, location: &LocationDescriptor) -> Option<&CachedBlock> {
        self.get_tagged(location, CacheCallSite::Chain)
    }

    /// Look up a cached block, tagging the lookup with its callsite for
    /// accounting purposes.
    pub fn get_tagged(
        &self,
        location: &LocationDescriptor,
        site: CacheCallSite,
    ) -> Option<&CachedBlock> {
        let lookups = self.lookups.fetch_add(1, Ordering::Relaxed) + 1;
        self.site_lookups[site.idx()].fetch_add(1, Ordering::Relaxed);
        let result = self.blocks.get(location);
        if result.is_some() {
            self.hits.fetch_add(1, Ordering::Relaxed);
            self.site_hits[site.idx()].fetch_add(1, Ordering::Relaxed);
        }
        // Periodic stats dump. Default 250 000 lookups. Override with
        // RUZU_JIT_STATS_INTERVAL=N (decimal). 0 disables.
        if jit_stats_enabled() {
            let interval = jit_stats_interval();
            if interval > 0 && lookups % interval == 0 {
                self.print_stats(&format!("interval@{}", lookups));
            }
        }
        result
    }

    /// Insert a compiled block into the cache.
    pub fn insert(&mut self, location: LocationDescriptor, block: CachedBlock) {
        self.inserts.fetch_add(1, Ordering::Relaxed);
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
        let removed = self.blocks.remove(location).is_some();
        if removed {
            self.invalidations.fetch_add(1, Ordering::Relaxed);
        }
        removed
    }

    /// Accumulate a compile-time sample (nanoseconds). One call per emit.
    pub fn add_compile_time(&self, ns: u64) {
        self.compile_time_ns.fetch_add(ns, Ordering::Relaxed);
        self.compile_count.fetch_add(1, Ordering::Relaxed);
    }

    /// Clear all cached blocks.
    pub fn clear(&mut self) {
        self.clears.fetch_add(1, Ordering::Relaxed);
        self.blocks.clear();
    }

    /// Invalidate blocks whose PC falls within [start, start+length).
    pub fn invalidate_range(&mut self, start: u64, length: u64) {
        let end = start.wrapping_add(length);
        let before = self.blocks.len();
        self.blocks.retain(|loc, _| {
            let pc = loc.value() & 0x00FF_FFFF_FFFF_FFFF; // PC mask (56 bits)
            pc < start || pc >= end
        });
        let removed = before.saturating_sub(self.blocks.len()) as u64;
        if removed > 0 {
            self.invalidations.fetch_add(removed, Ordering::Relaxed);
        }
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

    /// Snapshot of (lookups, hits, inserts, invalidations, clears).
    pub fn stats(&self) -> (u64, u64, u64, u64, u64) {
        (
            self.lookups.load(Ordering::Relaxed),
            self.hits.load(Ordering::Relaxed),
            self.inserts.load(Ordering::Relaxed),
            self.invalidations.load(Ordering::Relaxed),
            self.clears.load(Ordering::Relaxed),
        )
    }

    /// Bucket cached locations by their lower-32-bit PC component (works
    /// for both A32 hashes — PC is the low 32 bits — and A64 hashes —
    /// PC is the low 56 bits, so the low 32 are still PC bits). Returns
    /// `(unique_pcs, blocks_total, fragments, max_per_pc)` where
    /// `fragments = blocks_total - unique_pcs` (count of "extra" cache
    /// entries that share a PC with another entry, indicating cache
    /// fragmentation from upper-LD state bits).
    pub fn pc_fragmentation(&self) -> (usize, usize, usize, usize) {
        let mut buckets: HashMap<u32, usize> = HashMap::new();
        for loc in self.blocks.keys() {
            let pc = (loc.value() & 0xFFFF_FFFF) as u32;
            *buckets.entry(pc).or_insert(0) += 1;
        }
        let unique_pcs = buckets.len();
        let blocks_total = self.blocks.len();
        let fragments = blocks_total.saturating_sub(unique_pcs);
        let max_per_pc = buckets.values().copied().max().unwrap_or(0);
        (unique_pcs, blocks_total, fragments, max_per_pc)
    }

    /// Print formatted statistics to stderr, tagged with the cache id and
    /// caller-provided context.
    pub fn print_stats(&self, tag: &str) {
        let (lookups, hits, inserts, invalidations, clears) = self.stats();
        let misses = lookups.saturating_sub(hits);
        let hit_rate = if lookups > 0 {
            (hits as f64 / lookups as f64) * 100.0
        } else {
            0.0
        };
        eprintln!(
            "[BLOCK_CACHE_STATS cache_id={} {}] lookups={} hits={} ({:.2}%) misses={} inserts={} invalidations={} clears={} cached_blocks={}",
            self.id,
            tag,
            lookups,
            hits,
            hit_rate,
            misses,
            inserts,
            invalidations,
            clears,
            self.blocks.len(),
        );

        // Per-callsite breakdown.
        for site in [
            CacheCallSite::Lookup,
            CacheCallSite::Compile,
            CacheCallSite::Chain,
        ] {
            let l = self.site_lookups[site.idx()].load(Ordering::Relaxed);
            let h = self.site_hits[site.idx()].load(Ordering::Relaxed);
            let r = if l > 0 {
                (h as f64 / l as f64) * 100.0
            } else {
                0.0
            };
            eprintln!(
                "[BLOCK_CACHE_STATS cache_id={} {}.site_{}] lookups={} hits={} ({:.2}%) misses={}",
                self.id,
                tag,
                site.label(),
                l,
                h,
                r,
                l.saturating_sub(h),
            );
        }

        // Compile-time totals.
        let compile_count = self.compile_count.load(Ordering::Relaxed);
        let compile_ns = self.compile_time_ns.load(Ordering::Relaxed);
        let avg_us = if compile_count > 0 {
            (compile_ns as f64 / compile_count as f64) / 1_000.0
        } else {
            0.0
        };
        eprintln!(
            "[BLOCK_CACHE_STATS cache_id={} {}.compile] count={} total_ns={} total_ms={} avg_us={:.2}",
            self.id,
            tag,
            compile_count,
            compile_ns,
            compile_ns / 1_000_000,
            avg_us,
        );

        // PC fragmentation summary.
        let (unique_pcs, blocks_total, fragments, max_per_pc) = self.pc_fragmentation();
        let frag_pct = if blocks_total > 0 {
            (fragments as f64 / blocks_total as f64) * 100.0
        } else {
            0.0
        };
        eprintln!(
            "[BLOCK_CACHE_STATS cache_id={} {}.pc_frag] unique_pcs={} blocks={} fragments={} ({:.2}%) max_entries_per_pc={}",
            self.id,
            tag,
            unique_pcs,
            blocks_total,
            fragments,
            frag_pct,
            max_per_pc,
        );
    }
}

impl Default for BlockCache {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for BlockCache {
    fn drop(&mut self) {
        if std::env::var_os("RUZU_JIT_STATS").is_some() {
            self.print_stats("on_drop");
        }
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

    #[test]
    fn test_pc_fragmentation_no_fragments() {
        let mut cache = BlockCache::new();
        // 3 entries with distinct PCs in low 32 bits.
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
                entrypoint_offset: 0,
                size: 32,
            },
        );
        cache.insert(
            LocationDescriptor::new(0x3000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0,
                size: 32,
            },
        );
        let (unique, blocks, frag, max) = cache.pc_fragmentation();
        assert_eq!(unique, 3);
        assert_eq!(blocks, 3);
        assert_eq!(frag, 0);
        assert_eq!(max, 1);
    }

    #[test]
    fn test_pc_fragmentation_with_fragments() {
        let mut cache = BlockCache::new();
        // Two entries that share lower 32 bits (PC = 0x1000) but differ
        // in upper 32 bits (state bits).
        cache.insert(
            LocationDescriptor::new(0x0000_0000_0000_1000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0,
                size: 32,
            },
        );
        cache.insert(
            LocationDescriptor::new(0x0000_0001_0000_1000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0,
                size: 32,
            },
        );
        // Plus one with a unique PC.
        cache.insert(
            LocationDescriptor::new(0x2000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0,
                size: 32,
            },
        );
        let (unique, blocks, frag, max) = cache.pc_fragmentation();
        assert_eq!(unique, 2); // 0x1000 and 0x2000
        assert_eq!(blocks, 3);
        assert_eq!(frag, 1); // one extra entry on PC 0x1000
        assert_eq!(max, 2);
    }
}

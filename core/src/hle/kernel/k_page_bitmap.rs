//! Port of zuyu/src/core/hle/kernel/k_page_bitmap.h
//! Status: Ported (structures, stubbed RandomBitGenerator)
//! Derniere synchro: 2026-03-11
//!
//! Hierarchical page bitmap for the kernel page heap allocator.

/// Maximum depth of the hierarchical bitmap.
pub const MAX_DEPTH: usize = 4;

/// Number of bits in a u64.
const BITS_PER_U64: usize = 64;

// ---------------------------------------------------------------------------
// RandomBitGenerator (simplified — no TinyMT, uses basic RNG)
// ---------------------------------------------------------------------------

/// Simplified random bit generator. Upstream uses TinyMT; here we use a basic xorshift.
pub struct RandomBitGenerator {
    state: u64,
    entropy: u32,
    bits_available: u32,
}

impl RandomBitGenerator {
    pub fn new() -> Self {
        // Seed with a non-zero value.
        Self {
            state: 0x12345678_9ABCDEF0,
            entropy: 0,
            bits_available: 0,
        }
    }

    fn refresh_entropy(&mut self) {
        // Simple xorshift32 for entropy.
        let mut x = self.state as u32;
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        self.state = (self.state >> 32) | ((x as u64) << 32);
        self.entropy = x;
        self.bits_available = 32;
    }

    fn generate_random_bit(&mut self) -> bool {
        if self.bits_available == 0 {
            self.refresh_entropy();
        }
        let bit = (self.entropy & 1) != 0;
        self.entropy >>= 1;
        self.bits_available -= 1;
        bit
    }

    fn generate_random_bits(&mut self, mut num_bits: u32) -> u64 {
        let mut result: u64 = 0;
        while num_bits > 0 {
            if self.bits_available == 0 {
                self.refresh_entropy();
            }
            let cur_bits = num_bits.min(self.bits_available);
            let mask = (1u64 << cur_bits) - 1;
            result <<= cur_bits;
            result |= (self.entropy as u64) & mask;
            self.entropy >>= cur_bits;
            self.bits_available -= cur_bits;
            num_bits -= cur_bits;
        }
        result
    }

    pub fn select_random_bit(&mut self, mut bitmap: u64) -> u64 {
        let mut selected: u64 = 0;
        let mut cur_num_bits = BITS_PER_U64 / 2;
        while cur_num_bits != 0 {
            let high = bitmap >> cur_num_bits;
            let low = bitmap & !(!0u64 << cur_num_bits);
            if high != 0 && (low == 0 || self.generate_random_bit()) {
                bitmap = high;
                selected += cur_num_bits as u64;
            } else {
                bitmap = low;
            }
            cur_num_bits /= 2;
        }
        selected
    }

    pub fn generate_random(&mut self, max: u64) -> u64 {
        let bits_needed = if max == 0 {
            1
        } else {
            1 + (64 - max.leading_zeros())
        };
        let rnd = self.generate_random_bits(bits_needed);
        if max == 0 {
            0
        } else {
            rnd - (rnd / max) * max
        }
    }
}

impl Default for RandomBitGenerator {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// KPageBitmap
// ---------------------------------------------------------------------------

/// Port of Kernel::KPageBitmap.
///
/// Uses Vec<u64> instead of raw pointers for safety.
/// The hierarchical structure is the same: multiple depth levels of bitmaps.
pub struct KPageBitmap {
    bit_storages: [Vec<u64>; MAX_DEPTH],
    rng: RandomBitGenerator,
    num_bits: usize,
    used_depths: usize,
}

impl KPageBitmap {
    pub fn new() -> Self {
        Self {
            bit_storages: [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            rng: RandomBitGenerator::new(),
            num_bits: 0,
            used_depths: 0,
        }
    }

    pub fn get_num_bits(&self) -> usize {
        self.num_bits
    }

    pub fn get_highest_depth_index(&self) -> i32 {
        self.used_depths as i32 - 1
    }

    fn get_required_depth(mut region_size: usize) -> i32 {
        let mut depth = 0i32;
        loop {
            region_size /= BITS_PER_U64;
            depth += 1;
            if region_size == 0 {
                return depth;
            }
        }
    }

    /// Initialize the bitmap for a given number of entries.
    pub fn initialize(&mut self, size: usize) {
        self.num_bits = 0;
        self.used_depths = Self::get_required_depth(size) as usize;
        debug_assert!(self.used_depths <= MAX_DEPTH);

        let mut cur_size = size;
        for depth in (0..self.used_depths).rev() {
            cur_size = (cur_size + BITS_PER_U64 - 1) / BITS_PER_U64;
            self.bit_storages[depth] = vec![0u64; cur_size];
        }
    }

    pub fn find_free_block(&mut self, random: bool) -> i64 {
        let mut offset: usize = 0;
        let mut depth: i32 = 0;

        if random {
            loop {
                let v = self.bit_storages[depth as usize][offset];
                if v == 0 {
                    debug_assert!(depth == 0);
                    return -1;
                }
                offset = offset * BITS_PER_U64 + self.rng.select_random_bit(v) as usize;
                depth += 1;
                if depth >= self.used_depths as i32 {
                    break;
                }
            }
        } else {
            loop {
                let v = self.bit_storages[depth as usize][offset];
                if v == 0 {
                    debug_assert!(depth == 0);
                    return -1;
                }
                offset = offset * BITS_PER_U64 + v.trailing_zeros() as usize;
                depth += 1;
                if depth >= self.used_depths as i32 {
                    break;
                }
            }
        }

        offset as i64
    }

    pub fn find_free_range(&mut self, count: usize) -> i64 {
        if self.used_depths == 0 || count > BITS_PER_U64 {
            return -1;
        }
        let storage = &self.bit_storages[self.used_depths - 1];
        if storage.is_empty() {
            return -1;
        }
        let options_per_storage = (BITS_PER_U64 / count).max(1);
        let num_entries = storage.len().max(1);
        let free_mask = (1u64 << count) - 1;

        let mut num_valid_options: usize = 0;
        let mut chosen_offset: i64 = -1;

        for storage_index in 0..num_entries {
            let mut s = storage[storage_index];
            for option in 0..options_per_storage {
                if (s & free_mask) == free_mask {
                    num_valid_options += 1;
                    if num_valid_options == 1 || self.rng.generate_random(num_valid_options as u64) == 0
                    {
                        chosen_offset =
                            (storage_index * BITS_PER_U64 + option * count) as i64;
                    }
                }
                s >>= count;
            }
        }

        chosen_offset
    }

    pub fn set_bit(&mut self, offset: usize) {
        self.set_bit_at_depth(self.get_highest_depth_index(), offset);
        self.num_bits += 1;
    }

    pub fn clear_bit(&mut self, offset: usize) {
        self.clear_bit_at_depth(self.get_highest_depth_index(), offset);
        self.num_bits -= 1;
    }

    pub fn clear_range(&mut self, offset: usize, count: usize) -> bool {
        let depth = self.get_highest_depth_index();
        let bit_ind = offset / BITS_PER_U64;

        if count < BITS_PER_U64 {
            let shift = offset % BITS_PER_U64;
            debug_assert!(shift + count <= BITS_PER_U64);
            let mask = ((1u64 << count) - 1) << shift;
            let v = self.bit_storages[depth as usize][bit_ind];
            if (v & mask) != mask {
                return false;
            }
            let new_v = v & !mask;
            self.bit_storages[depth as usize][bit_ind] = new_v;
            if new_v == 0 {
                self.clear_bit_at_depth(depth - 1, bit_ind);
            }
        } else {
            debug_assert!(offset % BITS_PER_U64 == 0);
            debug_assert!(count % BITS_PER_U64 == 0);
            let words = count / BITS_PER_U64;
            for i in 0..words {
                if self.bit_storages[depth as usize][bit_ind + i] != !0u64 {
                    return false;
                }
            }
            for i in 0..words {
                self.bit_storages[depth as usize][bit_ind + i] = 0;
                self.clear_bit_at_depth(depth - 1, bit_ind + i);
            }
        }

        self.num_bits -= count;
        true
    }

    fn set_bit_at_depth(&mut self, mut depth: i32, mut offset: usize) {
        while depth >= 0 {
            let ind = offset / BITS_PER_U64;
            let which = offset % BITS_PER_U64;
            let mask = 1u64 << which;
            let v = self.bit_storages[depth as usize][ind];
            debug_assert!((v & mask) == 0);
            self.bit_storages[depth as usize][ind] = v | mask;
            if v != 0 {
                break;
            }
            offset = ind;
            depth -= 1;
        }
    }

    fn clear_bit_at_depth(&mut self, mut depth: i32, mut offset: usize) {
        while depth >= 0 {
            let ind = offset / BITS_PER_U64;
            let which = offset % BITS_PER_U64;
            let mask = 1u64 << which;
            let v = self.bit_storages[depth as usize][ind];
            debug_assert!((v & mask) != 0);
            let new_v = v & !mask;
            self.bit_storages[depth as usize][ind] = new_v;
            if new_v != 0 {
                break;
            }
            offset = ind;
            depth -= 1;
        }
    }

    pub fn calculate_management_overhead_size(mut region_size: usize) -> usize {
        let mut overhead_bits: usize = 0;
        let depth = Self::get_required_depth(region_size);
        for _ in (0..depth).rev() {
            region_size = (region_size + BITS_PER_U64 - 1) / BITS_PER_U64;
            overhead_bits += region_size;
        }
        overhead_bits * std::mem::size_of::<u64>()
    }
}

impl Default for KPageBitmap {
    fn default() -> Self {
        Self::new()
    }
}

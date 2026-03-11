//! Port of zuyu/src/core/hle/kernel/k_page_heap.h and k_page_heap.cpp
//! Status: Ported (structures and core methods)
//! Derniere synchro: 2026-03-11

use super::k_memory_block::PAGE_SIZE;
use super::k_page_bitmap::{KPageBitmap, RandomBitGenerator};

/// Number of memory block page shifts.
pub const NUM_MEMORY_BLOCK_PAGE_SHIFTS: usize = 7;

/// Memory block page shifts: 0xC, 0x10, 0x15, 0x16, 0x19, 0x1D, 0x1E
pub const MEMORY_BLOCK_PAGE_SHIFTS: [usize; NUM_MEMORY_BLOCK_PAGE_SHIFTS] =
    [0x0C, 0x10, 0x15, 0x16, 0x19, 0x1D, 0x1E];

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

/// A single block level in the page heap.
struct Block {
    bitmap: KPageBitmap,
    heap_address: u64,
    end_offset: usize,
    block_shift: usize,
    next_block_shift: usize,
}

impl Block {
    fn new() -> Self {
        Self {
            bitmap: KPageBitmap::new(),
            heap_address: 0,
            end_offset: 0,
            block_shift: 0,
            next_block_shift: 0,
        }
    }

    fn get_shift(&self) -> usize {
        self.block_shift
    }
    fn get_next_shift(&self) -> usize {
        self.next_block_shift
    }
    fn get_size(&self) -> usize {
        1usize << self.block_shift
    }
    fn get_num_pages(&self) -> usize {
        self.get_size() / PAGE_SIZE
    }
    fn get_num_free_blocks(&self) -> usize {
        self.bitmap.get_num_bits()
    }
    fn get_num_free_pages(&self) -> usize {
        self.get_num_free_blocks() * self.get_num_pages()
    }

    fn initialize(&mut self, addr: u64, size: usize, bs: usize, nbs: usize) {
        self.block_shift = bs;
        self.next_block_shift = nbs;

        let align = if nbs != 0 { 1u64 << nbs } else { 1u64 << bs };
        let aligned_addr = common::alignment::align_down(addr, align);
        let aligned_end =
            common::alignment::align_up(addr + size as u64, align);

        self.heap_address = aligned_addr;
        self.end_offset = ((aligned_end - aligned_addr) / (1u64 << bs)) as usize;
        self.bitmap.initialize(self.end_offset);
    }

    fn push_block(&mut self, address: u64) -> u64 {
        let offset = ((address - self.heap_address) >> self.get_shift()) as usize;
        self.bitmap.set_bit(offset);

        if self.get_next_shift() != 0 {
            let diff = 1usize << (self.get_next_shift() - self.get_shift());
            let aligned_offset = offset & !(diff - 1);
            if self.bitmap.clear_range(aligned_offset, diff) {
                return self.heap_address + ((aligned_offset as u64) << self.get_shift());
            }
        }

        0
    }

    fn pop_block(&mut self, random: bool) -> u64 {
        let soffset = self.bitmap.find_free_block(random);
        if soffset < 0 {
            return 0;
        }
        let offset = soffset as usize;
        self.bitmap.clear_bit(offset);
        self.heap_address + ((offset as u64) << self.get_shift())
    }

    fn calculate_management_overhead_size(
        region_size: usize,
        cur_block_shift: usize,
        next_block_shift: usize,
    ) -> usize {
        let cur_block_size = 1usize << cur_block_shift;
        let next_block_size = if next_block_shift != 0 {
            1usize << next_block_shift
        } else {
            cur_block_size
        };
        let align = if next_block_shift != 0 {
            next_block_size
        } else {
            cur_block_size
        };
        KPageBitmap::calculate_management_overhead_size(
            (align * 2 + common::alignment::align_up(region_size as u64, align as u64) as usize) / cur_block_size,
        )
    }
}

// ---------------------------------------------------------------------------
// KPageHeap
// ---------------------------------------------------------------------------

/// Port of Kernel::KPageHeap.
pub struct KPageHeap {
    heap_address: u64,
    heap_size: usize,
    initial_used_size: usize,
    num_blocks: usize,
    blocks: Vec<Block>,
    rng: RandomBitGenerator,
}

impl KPageHeap {
    pub fn new() -> Self {
        let mut blocks = Vec::with_capacity(NUM_MEMORY_BLOCK_PAGE_SHIFTS);
        for _ in 0..NUM_MEMORY_BLOCK_PAGE_SHIFTS {
            blocks.push(Block::new());
        }
        Self {
            heap_address: 0,
            heap_size: 0,
            initial_used_size: 0,
            num_blocks: 0,
            blocks,
            rng: RandomBitGenerator::new(),
        }
    }

    pub fn get_address(&self) -> u64 {
        self.heap_address
    }
    pub fn get_size(&self) -> usize {
        self.heap_size
    }
    pub fn get_end_address(&self) -> u64 {
        self.heap_address + self.heap_size as u64
    }
    pub fn get_page_offset(&self, block: u64) -> usize {
        ((block - self.heap_address) / PAGE_SIZE as u64) as usize
    }
    pub fn get_page_offset_to_end(&self, block: u64) -> usize {
        ((self.get_end_address() - block) / PAGE_SIZE as u64) as usize
    }

    pub fn initialize(&mut self, heap_address: u64, heap_size: usize) {
        self.initialize_with_shifts(
            heap_address,
            heap_size,
            &MEMORY_BLOCK_PAGE_SHIFTS,
            NUM_MEMORY_BLOCK_PAGE_SHIFTS,
        );
    }

    fn initialize_with_shifts(
        &mut self,
        heap_address: u64,
        heap_size: usize,
        block_shifts: &[usize],
        num_block_shifts: usize,
    ) {
        debug_assert!(heap_address % PAGE_SIZE as u64 == 0);
        debug_assert!(heap_size % PAGE_SIZE == 0);
        debug_assert!(num_block_shifts > 0 && num_block_shifts <= NUM_MEMORY_BLOCK_PAGE_SHIFTS);

        self.heap_address = heap_address;
        self.heap_size = heap_size;
        self.num_blocks = num_block_shifts;

        for i in 0..num_block_shifts {
            let cur_shift = block_shifts[i];
            let next_shift = if i != num_block_shifts - 1 {
                block_shifts[i + 1]
            } else {
                0
            };
            self.blocks[i].initialize(heap_address, heap_size, cur_shift, next_shift);
        }
    }

    pub fn get_free_size(&self) -> usize {
        self.get_num_free_pages() * PAGE_SIZE
    }

    pub fn get_num_free_pages(&self) -> usize {
        let mut num_free = 0;
        for i in 0..self.num_blocks {
            num_free += self.blocks[i].get_num_free_pages();
        }
        num_free
    }

    pub fn set_initial_used_size(&mut self, reserved_size: usize) {
        let free_size = self.get_num_free_pages() * PAGE_SIZE;
        debug_assert!(self.heap_size >= free_size + reserved_size);
        self.initial_used_size = self.heap_size - free_size - reserved_size;
    }

    pub fn allocate_block(&mut self, index: i32, random: bool) -> u64 {
        if random {
            let block_pages = self.blocks[index as usize].get_num_pages();
            self.allocate_by_random(index, block_pages, block_pages)
        } else {
            self.allocate_by_linear_search(index)
        }
    }

    pub fn allocate_aligned(&mut self, index: i32, num_pages: usize, align_pages: usize) -> u64 {
        self.allocate_by_random(index, num_pages, align_pages)
    }

    fn allocate_by_linear_search(&mut self, index: i32) -> u64 {
        let needed_size = self.blocks[index as usize].get_size();
        for i in index..self.num_blocks as i32 {
            let addr = self.blocks[i as usize].pop_block(false);
            if addr != 0 {
                let allocated_size = self.blocks[i as usize].get_size();
                if allocated_size > needed_size {
                    self.free(addr + needed_size as u64, (allocated_size - needed_size) / PAGE_SIZE);
                }
                return addr;
            }
        }
        0
    }

    fn allocate_by_random(&mut self, mut index: i32, num_pages: usize, align_pages: usize) -> u64 {
        let needed_size = num_pages * PAGE_SIZE;
        let align_size = align_pages * PAGE_SIZE;
        let align_shift = align_size.trailing_zeros() as usize;

        const MINIMUM_POSSIBLE_ALIGNMENTS: usize = 4;
        {
            let mut max_blocks = self.num_blocks as i32;
            let mut possible_alignments: usize = 0;
            for i in index..max_blocks {
                possible_alignments += (1 + ((self.blocks[i as usize].get_size() - needed_size)
                    >> align_shift))
                    * self.blocks[i as usize].get_num_free_blocks();
                if possible_alignments >= MINIMUM_POSSIBLE_ALIGNMENTS {
                    max_blocks = i + 1;
                    break;
                }
            }

            if possible_alignments > 0 && index + 1 < max_blocks {
                let rnd = self.rng.generate_random(possible_alignments as u64) as usize;
                possible_alignments = 0;
                for i in index..max_blocks {
                    possible_alignments += (1
                        + ((self.blocks[i as usize].get_size() - needed_size) >> align_shift))
                        * self.blocks[i as usize].get_num_free_blocks();
                    if rnd < possible_alignments {
                        index = i;
                        break;
                    }
                }
            }
        }

        let mut addr = self.blocks[index as usize].pop_block(true);
        if addr != 0 {
            let leftover_size = self.blocks[index as usize].get_size() - needed_size;
            if leftover_size > 0 {
                let possible_alignments = 1 + (leftover_size >> align_shift);
                let random_offset =
                    (self.rng.generate_random(possible_alignments as u64) as usize) << align_shift;

                if random_offset != 0 {
                    self.free(addr, random_offset / PAGE_SIZE);
                }
                addr += random_offset as u64;
                if random_offset != leftover_size {
                    self.free(
                        addr + needed_size as u64,
                        (leftover_size - random_offset) / PAGE_SIZE,
                    );
                }
            }
            return addr;
        }

        0
    }

    fn free_block(&mut self, mut block: u64, mut index: i32) {
        loop {
            block = self.blocks[index as usize].push_block(block);
            if block == 0 {
                break;
            }
            index += 1;
        }
    }

    pub fn free(&mut self, addr: u64, num_pages: usize) {
        if num_pages == 0 {
            return;
        }

        let mut big_index = self.num_blocks as i32 - 1;
        let start = addr;
        let end = addr + (num_pages * PAGE_SIZE) as u64;
        let mut before_start = start;
        let mut before_end = start;
        let mut after_start = end;
        let mut after_end = end;

        while big_index >= 0 {
            let block_size = self.blocks[big_index as usize].get_size() as u64;
            let big_start = common::alignment::align_up(start, block_size);
            let big_end =
                common::alignment::align_down(end, block_size);
            if big_start < big_end {
                let mut block = big_start;
                while block < big_end {
                    self.free_block(block, big_index);
                    block += block_size;
                }
                before_end = big_start;
                after_start = big_end;
                break;
            }
            big_index -= 1;
        }
        debug_assert!(big_index >= 0);

        for i in (0..big_index).rev() {
            let block_size = self.blocks[i as usize].get_size() as u64;
            while before_start + block_size <= before_end {
                before_end -= block_size;
                self.free_block(before_end, i);
            }
        }

        for i in (0..big_index).rev() {
            let block_size = self.blocks[i as usize].get_size() as u64;
            while after_start + block_size <= after_end {
                self.free_block(after_start, i);
                after_start += block_size;
            }
        }
    }

    pub fn get_aligned_block_index(num_pages: usize, align_pages: usize) -> i32 {
        let target_pages = num_pages.max(align_pages);
        for i in 0..NUM_MEMORY_BLOCK_PAGE_SHIFTS {
            if target_pages <= (1usize << MEMORY_BLOCK_PAGE_SHIFTS[i]) / PAGE_SIZE {
                return i as i32;
            }
        }
        -1
    }

    pub fn get_block_index(num_pages: usize) -> i32 {
        for i in (0..NUM_MEMORY_BLOCK_PAGE_SHIFTS as i32).rev() {
            if num_pages >= (1usize << MEMORY_BLOCK_PAGE_SHIFTS[i as usize]) / PAGE_SIZE {
                return i;
            }
        }
        -1
    }

    pub fn get_block_size(index: usize) -> usize {
        1usize << MEMORY_BLOCK_PAGE_SHIFTS[index]
    }

    pub fn get_block_num_pages(index: usize) -> usize {
        Self::get_block_size(index) / PAGE_SIZE
    }

    pub fn calculate_management_overhead_size(region_size: usize) -> usize {
        Self::calculate_management_overhead_size_with_shifts(
            region_size,
            &MEMORY_BLOCK_PAGE_SHIFTS,
            NUM_MEMORY_BLOCK_PAGE_SHIFTS,
        )
    }

    fn calculate_management_overhead_size_with_shifts(
        region_size: usize,
        block_shifts: &[usize],
        num_block_shifts: usize,
    ) -> usize {
        let mut overhead_size = 0;
        for i in 0..num_block_shifts {
            let cur = block_shifts[i];
            let next = if i != num_block_shifts - 1 {
                block_shifts[i + 1]
            } else {
                0
            };
            overhead_size += Block::calculate_management_overhead_size(region_size, cur, next);
        }
        common::alignment::align_up(overhead_size as u64, PAGE_SIZE as u64) as usize
    }
}

impl Default for KPageHeap {
    fn default() -> Self {
        Self::new()
    }
}

//! Stack frame layout used by the AArch64 host dispatcher.
//!
//! This mirrors upstream Dynarmic `backend/arm64/stack_layout.h`. The offsets
//! are consumed by generated ARM64 code, so layout drift is a correctness bug.

pub const SPILL_COUNT: usize = 64;
pub const RSB_COUNT: usize = 8;
pub const RSB_INDEX_MASK: u64 = ((RSB_COUNT - 1) * core::mem::size_of::<RSBEntry>()) as u64;

#[repr(C, align(16))]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct RSBEntry {
    pub target: u64,
    pub code_ptr: u64,
}

#[repr(C, align(16))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StackLayout {
    pub rsb: [RSBEntry; RSB_COUNT],
    pub spill: [[u64; 2]; SPILL_COUNT],
    pub rsb_ptr: u32,
    _pad_after_rsb_ptr: u32,
    pub cycles_to_run: i64,
    pub save_host_fpcr: u32,
    pub check_bit: bool,
    _pad_tail: [u8; 11],
}

impl StackLayout {
    pub const fn rsb_offset() -> usize {
        core::mem::offset_of!(StackLayout, rsb)
    }

    pub const fn rsb_entry_offset(index: usize) -> usize {
        core::mem::offset_of!(StackLayout, rsb) + index * core::mem::size_of::<RSBEntry>()
    }

    pub const fn spill_offset(index: usize) -> usize {
        core::mem::offset_of!(StackLayout, spill) + index * 16
    }

    pub const fn rsb_ptr_offset() -> usize {
        core::mem::offset_of!(StackLayout, rsb_ptr)
    }

    pub const fn cycles_to_run_offset() -> usize {
        core::mem::offset_of!(StackLayout, cycles_to_run)
    }

    pub const fn save_host_fpcr_offset() -> usize {
        core::mem::offset_of!(StackLayout, save_host_fpcr)
    }

    pub const fn check_bit_offset() -> usize {
        core::mem::offset_of!(StackLayout, check_bit)
    }
}

impl Default for StackLayout {
    fn default() -> Self {
        Self {
            rsb: [RSBEntry::default(); RSB_COUNT],
            spill: [[0; 2]; SPILL_COUNT],
            rsb_ptr: 0,
            _pad_after_rsb_ptr: 0,
            cycles_to_run: 0,
            save_host_fpcr: 0,
            check_bit: false,
            _pad_tail: [0; 11],
        }
    }
}

const _: () = assert!(core::mem::size_of::<StackLayout>().is_multiple_of(16));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rsb_entry_layout_matches_upstream() {
        assert_eq!(core::mem::align_of::<RSBEntry>(), 16);
        assert_eq!(core::mem::size_of::<RSBEntry>(), 16);
        assert_eq!(core::mem::offset_of!(RSBEntry, target), 0);
        assert_eq!(core::mem::offset_of!(RSBEntry, code_ptr), 8);
    }

    #[test]
    fn stack_layout_matches_upstream() {
        assert_eq!(SPILL_COUNT, 64);
        assert_eq!(RSB_COUNT, 8);
        assert_eq!(RSB_INDEX_MASK, 112);

        assert_eq!(core::mem::align_of::<StackLayout>(), 16);
        assert_eq!(core::mem::size_of::<StackLayout>(), 1184);
        assert_eq!(StackLayout::rsb_offset(), 0);
        assert_eq!(StackLayout::spill_offset(0), 128);
        assert_eq!(StackLayout::spill_offset(1), 144);
        assert_eq!(StackLayout::spill_offset(SPILL_COUNT - 1), 1136);
        assert_eq!(StackLayout::rsb_ptr_offset(), 1152);
        assert_eq!(StackLayout::cycles_to_run_offset(), 1160);
        assert_eq!(StackLayout::save_host_fpcr_offset(), 1168);
        assert_eq!(StackLayout::check_bit_offset(), 1172);
    }
}

//! ARM64 FPSR lazy load/spill helper.
//!
//! Upstream owner: `backend/arm64/fpsr_manager.h/.cpp`.

use super::abi::{XSCRATCH0, XSCRATCH1, XSTATE};
use super::block_of_code::BlockOfCode;
use super::inst;

#[derive(Debug)]
pub struct FpsrManager {
    state_fpsr_offset: usize,
    fpsr_loaded: bool,
}

impl FpsrManager {
    pub fn new(state_fpsr_offset: usize) -> Self {
        Self {
            state_fpsr_offset,
            fpsr_loaded: false,
        }
    }

    pub fn spill(&mut self, code: &mut BlockOfCode) -> Result<(), String> {
        if !self.fpsr_loaded {
            return Ok(());
        }

        let offset = u32::try_from(self.state_fpsr_offset).map_err(|_| {
            format!(
                "ARM64 FPSR state offset does not fit in u32: {}",
                self.state_fpsr_offset
            )
        })?;
        code.write_u32(inst::ldr_w_unsigned(XSCRATCH0, XSTATE, offset))?;
        code.write_u32(inst::mrs_fpsr(XSCRATCH1))?;
        code.write_u32(inst::orr_w(XSCRATCH0, XSCRATCH0, XSCRATCH1))?;
        code.write_u32(inst::str_w_unsigned(XSCRATCH0, XSTATE, offset))?;

        self.fpsr_loaded = false;
        Ok(())
    }

    pub fn load(&mut self, code: &mut BlockOfCode) -> Result<(), String> {
        if self.fpsr_loaded {
            return Ok(());
        }

        code.write_u32(inst::msr_fpsr(31))?;
        self.fpsr_loaded = true;
        Ok(())
    }

    pub fn overwrite(&mut self) {
        self.fpsr_loaded = false;
    }

    pub fn is_loaded(&self) -> bool {
        self.fpsr_loaded
    }
}

impl Default for FpsrManager {
    fn default() -> Self {
        Self::new(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn emitted_words(code: &BlockOfCode) -> Vec<u32> {
        let count = code.code_size() / core::mem::size_of::<u32>();
        (0..count)
            .map(|index| unsafe {
                code.code_base_ptr()
                    .add(index * core::mem::size_of::<u32>())
                    .cast::<u32>()
                    .read_unaligned()
            })
            .collect()
    }

    #[test]
    fn load_emits_fpsr_clear_once() {
        let mut code = BlockOfCode::with_size(4096).unwrap();
        let mut fpsr = FpsrManager::new(12);

        fpsr.load(&mut code).unwrap();
        fpsr.load(&mut code).unwrap();

        assert!(fpsr.is_loaded());
        assert_eq!(emitted_words(&code), vec![inst::msr_fpsr(31)]);
    }

    #[test]
    fn spill_emits_upstream_sequence_only_when_loaded() {
        let mut code = BlockOfCode::with_size(4096).unwrap();
        let mut fpsr = FpsrManager::new(12);

        fpsr.spill(&mut code).unwrap();
        assert_eq!(code.code_size(), 0);

        fpsr.load(&mut code).unwrap();
        fpsr.spill(&mut code).unwrap();
        fpsr.spill(&mut code).unwrap();

        assert!(!fpsr.is_loaded());
        assert_eq!(
            emitted_words(&code),
            vec![
                inst::msr_fpsr(31),
                inst::ldr_w_unsigned(XSCRATCH0, XSTATE, 12),
                inst::mrs_fpsr(XSCRATCH1),
                inst::orr_w(XSCRATCH0, XSCRATCH0, XSCRATCH1),
                inst::str_w_unsigned(XSCRATCH0, XSTATE, 12),
            ]
        );
    }

    #[test]
    fn overwrite_marks_fpsr_not_loaded_without_emitting() {
        let mut code = BlockOfCode::with_size(4096).unwrap();
        let mut fpsr = FpsrManager::new(12);

        fpsr.load(&mut code).unwrap();
        fpsr.overwrite();
        fpsr.spill(&mut code).unwrap();

        assert!(!fpsr.is_loaded());
        assert_eq!(emitted_words(&code), vec![inst::msr_fpsr(31)]);
    }
}

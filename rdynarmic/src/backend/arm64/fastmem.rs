//! ARM64 fastmem manager.
//!
//! Upstream owner: `backend/arm64/fastmem.h`.

use std::collections::HashSet;

use crate::ir::location::LocationDescriptor;

pub type DoNotFastmemMarker = (LocationDescriptor, u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FakeCall {
    pub call: *const u8,
    pub ret: *const u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FastmemPatchInfo {
    pub marker: DoNotFastmemMarker,
    pub fc: FakeCall,
    pub recompile: bool,
}

pub trait ExceptionHandler {
    fn supports_fastmem(&self) -> bool;
}

#[derive(Debug, Default)]
pub struct NullExceptionHandler;

impl ExceptionHandler for NullExceptionHandler {
    fn supports_fastmem(&self) -> bool {
        false
    }
}

pub struct FastmemManager<'a> {
    exception_handler: &'a dyn ExceptionHandler,
    do_not_fastmem: HashSet<DoNotFastmemMarker>,
}

impl<'a> FastmemManager<'a> {
    pub fn new(exception_handler: &'a dyn ExceptionHandler) -> Self {
        Self {
            exception_handler,
            do_not_fastmem: HashSet::new(),
        }
    }

    pub fn supports_fastmem(&self) -> bool {
        self.exception_handler.supports_fastmem()
    }

    pub fn should_fastmem(&self, marker: DoNotFastmemMarker) -> bool {
        !self.do_not_fastmem.contains(&marker)
    }

    pub fn mark_do_not_fastmem(&mut self, marker: DoNotFastmemMarker) {
        self.do_not_fastmem.insert(marker);
    }

    pub fn do_not_fastmem_count(&self) -> usize {
        self.do_not_fastmem.len()
    }
}

impl Default for FastmemManager<'static> {
    fn default() -> Self {
        static NULL_EXCEPTION_HANDLER: NullExceptionHandler = NullExceptionHandler;
        Self::new(&NULL_EXCEPTION_HANDLER)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct TestExceptionHandler(bool);

    impl ExceptionHandler for TestExceptionHandler {
        fn supports_fastmem(&self) -> bool {
            self.0
        }
    }

    #[test]
    fn forwards_supports_fastmem_to_exception_handler() {
        let supported = TestExceptionHandler(true);
        let unsupported = TestExceptionHandler(false);

        assert!(FastmemManager::new(&supported).supports_fastmem());
        assert!(!FastmemManager::new(&unsupported).supports_fastmem());
    }

    #[test]
    fn marker_blocks_fastmem_after_mark_do_not_fastmem() {
        let handler = TestExceptionHandler(true);
        let mut manager = FastmemManager::new(&handler);
        let marker = (LocationDescriptor::new(0x1234_5678), 7);
        let other_inst = (LocationDescriptor::new(0x1234_5678), 8);
        let other_location = (LocationDescriptor::new(0x1234_5679), 7);

        assert!(manager.should_fastmem(marker));
        manager.mark_do_not_fastmem(marker);

        assert!(!manager.should_fastmem(marker));
        assert!(manager.should_fastmem(other_inst));
        assert!(manager.should_fastmem(other_location));
        assert_eq!(manager.do_not_fastmem_count(), 1);
    }

    #[test]
    fn default_exception_handler_disables_fastmem() {
        let manager = FastmemManager::default();
        assert!(!manager.supports_fastmem());
    }
}

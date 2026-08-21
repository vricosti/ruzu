//! Executable code cache for the AArch64 host backend.
//!
//! This is the ARM64 counterpart to the x64 backend's `BlockOfCode` ownership
//! of executable memory. It deliberately contains no guest IR emission yet:
//! the first parity step is getting allocation, write protection, and I-cache
//! invalidation correct for Apple Silicon.

use std::ffi::c_void;
use std::ptr::NonNull;

use super::inst;

/// Default code cache size. Upstream uses a smaller cache on ARM64 hosts than
/// on x64 hosts to reduce pressure in constrained address spaces.
pub const DEFAULT_CODE_SIZE: usize = 128 * 1024 * 1024;

/// AArch64 instructions are fixed-width 32-bit words.
pub const INSTRUCTION_SIZE: usize = 4;

/// Initial ARM64 dispatcher function type.
///
/// Arguments match the existing x64 dispatcher shape: `(jit_state, code_ptr)`.
/// The first bootstrap stub ignores `jit_state` and calls `code_ptr`.
pub type RunCodeFn = unsafe extern "C" fn(*mut c_void, *const u8) -> u32;

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
unsafe extern "C" {
    fn pthread_jit_write_protect_np(enabled: i32);
    fn sys_icache_invalidate(start: *mut c_void, len: usize);
}

/// Executable AArch64 code buffer.
pub struct BlockOfCode {
    ptr: NonNull<u8>,
    size: usize,
    cursor: usize,
    jit_write_protection_enabled: bool,
}

impl BlockOfCode {
    /// Allocate an executable code cache with the default ARM64 size.
    pub fn new() -> Result<Self, String> {
        Self::with_size(DEFAULT_CODE_SIZE)
    }

    /// Allocate an executable code cache.
    pub fn with_size(size: usize) -> Result<Self, String> {
        if size == 0 {
            return Err("ARM64 code cache size must be non-zero".to_string());
        }

        let ptr = unsafe { mmap_code_cache(size)? };
        Ok(Self {
            ptr,
            size,
            cursor: 0,
            jit_write_protection_enabled: true,
        })
    }

    /// Write one raw AArch64 instruction and return its byte offset.
    pub fn write_u32(&mut self, instruction: u32) -> Result<usize, String> {
        if self.cursor % INSTRUCTION_SIZE != 0 {
            return Err(format!(
                "ARM64 code cursor is not instruction-aligned: {}",
                self.cursor
            ));
        }
        if self.cursor + INSTRUCTION_SIZE > self.size {
            return Err("ARM64 code cache exhausted".to_string());
        }

        let offset = self.cursor;
        unsafe {
            self.set_jit_write_protection(false);
            self.ptr
                .as_ptr()
                .add(offset)
                .cast::<u32>()
                .write_unaligned(instruction);
        }
        self.cursor += INSTRUCTION_SIZE;
        Ok(offset)
    }

    /// Write one 64-bit data word into the code cache and return its byte offset.
    pub fn write_u64(&mut self, value: u64) -> Result<usize, String> {
        if self.cursor % core::mem::align_of::<u64>() != 0 {
            return Err(format!(
                "ARM64 data cursor is not 8-byte aligned: {}",
                self.cursor
            ));
        }
        if self.cursor + core::mem::size_of::<u64>() > self.size {
            return Err("ARM64 code cache exhausted".to_string());
        }

        let offset = self.cursor;
        unsafe {
            self.set_jit_write_protection(false);
            self.ptr
                .as_ptr()
                .add(offset)
                .cast::<u64>()
                .write_unaligned(value);
        }
        self.cursor += core::mem::size_of::<u64>();
        Ok(offset)
    }

    /// Align the write cursor by emitting NOP padding.
    pub fn align(&mut self, alignment: usize) -> Result<(), String> {
        if alignment == 0 || !alignment.is_power_of_two() {
            return Err(format!("ARM64 invalid code alignment: {alignment}"));
        }
        while self.cursor % alignment != 0 {
            self.write_u32(inst::nop())?;
        }
        Ok(())
    }

    /// Patch one existing AArch64 instruction in place.
    pub fn patch_u32(&mut self, offset: usize, instruction: u32) -> Result<(), String> {
        self.patch_u32_inner(offset, instruction, true)
    }

    /// Patch one existing AArch64 instruction and defer I-cache invalidation.
    ///
    /// This matches upstream `AddressSpace::Emit`: code memory is made
    /// writable once, all link/relink patches are applied, then the modified
    /// ranges are invalidated in one batch before code memory is protected.
    pub(crate) fn patch_u32_deferred_icache(
        &mut self,
        offset: usize,
        instruction: u32,
    ) -> Result<(), String> {
        self.patch_u32_inner(offset, instruction, false)
    }

    fn patch_u32_inner(
        &mut self,
        offset: usize,
        instruction: u32,
        flush_icache: bool,
    ) -> Result<(), String> {
        if offset % INSTRUCTION_SIZE != 0 {
            return Err(format!(
                "ARM64 patch offset is not instruction-aligned: {offset}"
            ));
        }
        if offset + INSTRUCTION_SIZE > self.size {
            return Err(format!(
                "ARM64 patch offset out of code cache range: {offset}"
            ));
        }

        let was_protected = self.jit_write_protection_enabled;
        unsafe {
            self.set_jit_write_protection(false);
            self.ptr
                .as_ptr()
                .add(offset)
                .cast::<u32>()
                .write_unaligned(instruction);
            if flush_icache {
                flush_instruction_cache(self.ptr.as_ptr().add(offset), INSTRUCTION_SIZE);
            }
            self.set_jit_write_protection(was_protected);
        }
        Ok(())
    }

    /// Disable per-thread JIT write protection for a batch of code-cache
    /// writes. Upstream `AddressSpace::Emit` calls `UnprotectCodeMemory()`
    /// once before emitting/linking a block and protects again after the
    /// emitted range has been invalidated.
    pub fn unprotect(&mut self) {
        unsafe {
            self.set_jit_write_protection(false);
        }
    }

    /// Flush the host I-cache for the emitted code and make the buffer
    /// executable on platforms with per-thread JIT write protection.
    pub fn seal(&mut self) {
        self.seal_range(0, self.cursor);
    }

    /// Flush a specific emitted code range and make the buffer executable.
    ///
    /// Upstream `AddressSpace::Emit` invalidates only the newly emitted block
    /// (`mem.invalidate(entry_point, size)`) before re-enabling code-memory
    /// protection. Keeping this range-based entry point avoids flushing the
    /// entire growing cache after every block.
    pub fn seal_range(&mut self, offset: usize, len: usize) {
        self.seal_ranges(&[(offset, len)]);
    }

    /// Flush multiple emitted/relocated ranges and make the buffer executable.
    pub fn seal_ranges(&mut self, ranges: &[(usize, usize)]) {
        assert!(
            ranges
                .iter()
                .all(|(offset, len)| *offset <= self.size
                    && *len <= self.size.saturating_sub(*offset)),
            "ARM64 seal ranges out of code cache bounds: ranges={ranges:?} size={}",
            self.size
        );
        unsafe {
            for (offset, len) in ranges {
                flush_instruction_cache(self.ptr.as_ptr().add(*offset), *len);
            }
            self.set_jit_write_protection(true);
        }
    }

    /// Clear emitted code while keeping the allocation alive.
    pub fn clear_cache(&mut self) {
        self.cursor = 0;
    }

    /// Reset the write cursor to an existing code offset.
    pub fn set_code_size(&mut self, offset: usize) -> Result<(), String> {
        if offset > self.size {
            return Err(format!(
                "ARM64 code cursor out of code cache range: {offset} > {}",
                self.size
            ));
        }
        if offset % INSTRUCTION_SIZE != 0 {
            return Err(format!(
                "ARM64 code cursor is not instruction-aligned: {offset}"
            ));
        }
        self.cursor = offset;
        Ok(())
    }

    pub fn code_base_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }

    pub fn code_size(&self) -> usize {
        self.cursor
    }

    pub fn total_size(&self) -> usize {
        self.size
    }

    pub fn space_remaining(&self) -> usize {
        self.size.saturating_sub(self.cursor)
    }

    unsafe fn set_jit_write_protection(&mut self, enabled: bool) {
        if self.jit_write_protection_enabled == enabled {
            return;
        }
        set_jit_write_protect(enabled);
        self.jit_write_protection_enabled = enabled;
    }

    /// Emit the first ARM64 dispatcher bootstrap:
    ///
    /// ```text
    /// stp x29, x30, [sp, #-16]!
    /// blr x1
    /// ldp x29, x30, [sp], #16
    /// ret
    /// ```
    ///
    /// This is not the full dynarmic dispatcher yet; it proves the host ABI,
    /// executable cache, LR preservation, and branch-to-compiled-block path
    /// that the real dispatcher will extend with JitState and HaltReason logic.
    pub fn emit_direct_run_code_stub(&mut self) -> Result<RunCodeFn, String> {
        if self.cursor != 0 {
            return Err("ARM64 run_code stub must start at code offset 0".to_string());
        }
        self.write_u32(inst::stp_fp_lr_pre_16())?;
        self.write_u32(inst::blr(1))?;
        self.write_u32(inst::ldp_fp_lr_post_16())?;
        self.write_u32(inst::ret_lr())?;
        self.seal();

        Ok(unsafe { std::mem::transmute(self.code_base_ptr()) })
    }
}

impl Drop for BlockOfCode {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.ptr.as_ptr().cast::<c_void>(), self.size);
        }
    }
}

unsafe fn mmap_code_cache(size: usize) -> Result<NonNull<u8>, String> {
    #[cfg(target_os = "macos")]
    let flags = libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_JIT;
    #[cfg(not(target_os = "macos"))]
    let flags = libc::MAP_PRIVATE | libc::MAP_ANON;

    let ptr = libc::mmap(
        std::ptr::null_mut(),
        size,
        libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
        flags,
        -1,
        0,
    );
    if ptr == libc::MAP_FAILED {
        return Err(format!(
            "failed to allocate ARM64 executable code cache: {}",
            std::io::Error::last_os_error()
        ));
    }

    NonNull::new(ptr.cast::<u8>()).ok_or_else(|| "mmap returned null".to_string())
}

unsafe fn set_jit_write_protect(enabled: bool) {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    pthread_jit_write_protect_np(enabled as i32);

    #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
    let _ = enabled;
}

unsafe fn flush_instruction_cache(ptr: *mut u8, len: usize) {
    if len == 0 {
        return;
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    sys_icache_invalidate(ptr.cast::<c_void>(), len);

    #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
    {
        let _ = ptr;
        let _ = len;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::arm64::inst;

    #[test]
    fn block_of_code_tracks_offsets_and_capacity() {
        let mut code = BlockOfCode::with_size(4096).expect("code cache");
        assert_eq!(code.code_size(), 0);
        assert_eq!(code.space_remaining(), 4096);
        assert_eq!(code.write_u32(inst::nop()).unwrap(), 0);
        assert_eq!(code.write_u32(inst::ret_lr()).unwrap(), 4);
        assert_eq!(code.code_size(), 8);
        assert_eq!(code.space_remaining(), 4088);
        code.patch_u32(0, inst::ret_lr()).unwrap();
        let patched = unsafe { code.code_base_ptr().cast::<u32>().read_unaligned() };
        assert_eq!(patched, inst::ret_lr());
        assert!(code.patch_u32(2, inst::nop()).is_err());
        code.seal();
        code.clear_cache();
        assert_eq!(code.code_size(), 0);
        code.set_code_size(8).unwrap();
        assert_eq!(code.code_size(), 8);
        assert!(code.set_code_size(7).is_err());
    }

    #[test]
    fn writes_aligned_data_words_and_padding() {
        let mut code = BlockOfCode::with_size(4096).expect("code cache");
        code.write_u32(inst::nop()).unwrap();
        code.align(8).unwrap();
        let data_offset = code.write_u64(0x0123_4567_89ab_cdef).unwrap();

        assert_eq!(data_offset, 8);
        let data = unsafe {
            code.code_base_ptr()
                .add(data_offset)
                .cast::<u64>()
                .read_unaligned()
        };
        assert_eq!(data, 0x0123_4567_89ab_cdef);
        assert_eq!(code.code_size(), 16);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn emitted_arm64_code_executes() {
        let mut code = BlockOfCode::with_size(4096).expect("code cache");
        code.write_u32(inst::movz_x(0, 0x1234, 0)).unwrap();
        code.write_u32(inst::ret_lr()).unwrap();
        code.seal();

        let func: extern "C" fn() -> u64 = unsafe { std::mem::transmute(code.code_base_ptr()) };
        assert_eq!(func(), 0x1234);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn direct_run_code_stub_calls_block_pointer() {
        let mut block = BlockOfCode::with_size(4096).expect("block code cache");
        block.write_u32(inst::movz_x(0, 0x55aa, 0)).unwrap();
        block.write_u32(inst::ret_lr()).unwrap();
        block.seal();

        let mut dispatcher = BlockOfCode::with_size(4096).expect("dispatcher code cache");
        let run_code = dispatcher.emit_direct_run_code_stub().unwrap();
        let result = unsafe { run_code(std::ptr::null_mut(), block.code_base_ptr()) };
        assert_eq!(result, 0x55aa);
    }
}

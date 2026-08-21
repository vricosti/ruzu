use rxbyak::RegExp;
use std::collections::HashMap;

/// A 128-bit constant value (lower, upper).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Constant(pub u64, pub u64);

/// A pool of deduplicated 128-bit constants allocated in the code cache.
///
/// Matches upstream dynarmic's ConstantPool (constant_pool.h/cpp):
/// - Constants are placed into a pre-allocated region within the executable
///   code buffer, allocated once in the constructor.
/// - `get_constant()` returns a RIP-relative `RegExp` address for direct use
///   in `movaps xmm, xmmword_ptr(addr)`, matching upstream's contract where
///   `GetConstant(frame, lower, upper)` returns an `Xbyak::Address`.
///   The `RegExp` uses `rip_addr()` so the encoder computes the displacement
///   at emit time, matching Xbyak's `code.rip + void_ptr` (isAddr_=true).
/// - The pool is never cleared on cache reset — upstream's `ClearCache()`
///   only rewinds the code pointer, not the constant pool.
/// - Pool exhaustion is an assert, matching upstream ASSERT.
///
/// **Ownership adaptation (Rust):** upstream's ConstantPool owns its
/// allocation from BlockOfCode in the constructor. In Rust, the pool is
/// constructed unbound and `set_pool_base()` is called once after the
/// assembler reserves space. This is a valid Rust adaptation — the pool
/// base is stable for the BlockOfCode lifetime.
pub struct ConstantPool {
    /// Map from constant value to its byte offset within the pool.
    constants: HashMap<Constant, usize>,
    /// Next insertion offset (in bytes).
    insertion_point: usize,
    /// Maximum pool size in bytes.
    max_size: usize,
    /// Base pointer of the pool in the code cache.
    /// Set once via `set_pool_base` after the assembler reserves space.
    pool_base: *mut u8,
}

// Safety: pool_base points into the code cache which is stable for the JIT's lifetime.
unsafe impl Send for ConstantPool {}

impl ConstantPool {
    /// Create a new constant pool with the given capacity in bytes.
    /// `set_pool_base` must be called before use.
    pub fn new(size_bytes: usize) -> Self {
        Self {
            constants: HashMap::new(),
            insertion_point: 0,
            max_size: size_bytes,
            pool_base: std::ptr::null_mut(),
        }
    }

    /// Set the base pointer of the pool in the code cache.
    /// Called once after the assembler has reserved space for the pool.
    pub fn set_pool_base(&mut self, base: *mut u8) {
        self.pool_base = base;
    }

    /// Get or insert a 128-bit constant, returning a RIP-relative `RegExp`
    /// address suitable for `movaps xmm, xmmword_ptr(addr)`.
    ///
    /// Matches upstream `ConstantPool::GetConstant(frame, lower, upper)`
    /// which returns `frame[code.rip + pointer_to_constant]`.
    ///
    /// The returned `RegExp` uses `rip_addr()` — the encoder computes
    /// the correct RIP-relative displacement at emit time. No instruction
    /// size guessing needed.
    ///
    /// Panics if the pool is full (matches upstream ASSERT).
    pub fn get_constant(&mut self, lower: u64, upper: u64) -> RegExp {
        let const_ptr = self.get_or_insert(lower, upper);
        RegExp::rip_addr(const_ptr as i64)
    }

    /// Get or insert a constant, returning a raw pointer to it in the code cache.
    /// Panics if the pool is full, matching upstream `ASSERT(insertion_point < pool.size())`.
    fn get_or_insert(&mut self, lower: u64, upper: u64) -> *const u8 {
        assert!(!self.pool_base.is_null(), "Pool base not set");

        let constant = Constant(lower, upper);
        if let Some(&offset) = self.constants.get(&constant) {
            return unsafe { self.pool_base.add(offset) };
        }

        let offset = self.insertion_point;
        assert!(
            offset + 16 <= self.max_size,
            "Constant pool exhausted (used {}/{} bytes)",
            offset + 16,
            self.max_size
        );

        // Write the constant to the code cache.
        unsafe {
            let ptr = self.pool_base.add(offset);
            std::ptr::write_unaligned(ptr as *mut u64, lower);
            std::ptr::write_unaligned(ptr.add(8) as *mut u64, upper);
        }

        self.insertion_point += 16;
        self.constants.insert(constant, offset);
        unsafe { self.pool_base.add(offset) }
    }

    /// Number of constants currently stored.
    pub fn len(&self) -> usize {
        self.constants.len()
    }

    /// Whether the pool is empty.
    pub fn is_empty(&self) -> bool {
        self.constants.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_dedup() {
        let mut buf = vec![0u8; 1024];
        let mut pool = ConstantPool::new(1024);
        pool.set_pool_base(buf.as_mut_ptr());
        let p1 = pool.get_or_insert(0x1234, 0x5678);
        let p2 = pool.get_or_insert(0x1234, 0x5678);
        assert_eq!(p1, p2, "Same constant should return same pointer");
        assert_eq!(pool.len(), 1);
    }

    #[test]
    fn test_different_constants() {
        let mut buf = vec![0u8; 1024];
        let mut pool = ConstantPool::new(1024);
        pool.set_pool_base(buf.as_mut_ptr());
        let p1 = pool.get_or_insert(0xAAAA, 0);
        let p2 = pool.get_or_insert(0xBBBB, 0);
        assert_ne!(p1, p2);
        assert_eq!(pool.len(), 2);
    }

    #[test]
    #[should_panic(expected = "Constant pool exhausted")]
    fn test_pool_exhaustion_panics() {
        let mut buf = vec![0u8; 32];
        let mut pool = ConstantPool::new(32); // 2 entries max
        pool.set_pool_base(buf.as_mut_ptr());
        pool.get_or_insert(1, 0);
        pool.get_or_insert(2, 0);
        pool.get_or_insert(3, 0); // should panic
    }

    #[test]
    fn test_value_written() {
        let mut buf = vec![0u8; 1024];
        let mut pool = ConstantPool::new(1024);
        pool.set_pool_base(buf.as_mut_ptr());
        let ptr = pool.get_or_insert(0x0102_0304_0506_0708, 0x090A_0B0C_0D0E_0F10);
        let lower = unsafe { std::ptr::read_unaligned(ptr as *const u64) };
        let upper = unsafe { std::ptr::read_unaligned(ptr.add(8) as *const u64) };
        assert_eq!(lower, 0x0102_0304_0506_0708);
        assert_eq!(upper, 0x090A_0B0C_0D0E_0F10);
    }

    #[test]
    fn test_get_constant_returns_rip_addr() {
        let mut buf = vec![0u8; 1024];
        let mut pool = ConstantPool::new(1024);
        pool.set_pool_base(buf.as_mut_ptr());
        let addr = pool.get_constant(0x3F800000, 0);
        // Should be RIP-relative with is_addr=true
        assert!(addr.is_rip());
    }
}

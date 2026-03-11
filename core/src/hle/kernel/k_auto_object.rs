//! Port of zuyu/src/core/hle/kernel/k_auto_object.h and k_auto_object.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! Base class for all kernel objects. Provides reference counting, type identification
//! via class tokens, and the KScopedAutoObject RAII wrapper.
//!
//! In C++ this is a virtual base class with KERNEL_AUTOOBJECT_TRAITS macros.
//! In Rust we use a trait (KAutoObjectBase) for the virtual interface.

use std::sync::atomic::{AtomicU32, Ordering};

use super::k_class_token::{self, ClassTokenType};

/// TypeObj — mirrors the C++ KAutoObject::TypeObj inner class.
/// Holds a type name and class token for runtime type identification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeObj {
    m_name: &'static str,
    m_class_token: ClassTokenType,
}

impl TypeObj {
    pub const fn new(name: &'static str, class_token: ClassTokenType) -> Self {
        Self {
            m_name: name,
            m_class_token: class_token,
        }
    }

    pub const fn get_name(&self) -> &'static str {
        self.m_name
    }

    pub const fn get_class_token(&self) -> ClassTokenType {
        self.m_class_token
    }

    pub const fn is_derived_from(&self, rhs: &TypeObj) -> bool {
        (self.m_class_token | rhs.m_class_token) == self.m_class_token
    }
}

/// Trait representing the virtual interface of KAutoObject.
/// C++ uses virtual methods + KERNEL_AUTOOBJECT_TRAITS macro.
/// In Rust, every kernel object type implements this trait.
pub trait KAutoObjectBase {
    /// Returns the static TypeObj for this concrete type.
    fn get_type_obj(&self) -> TypeObj;

    /// Returns the static type name for this concrete type.
    fn get_type_name(&self) -> &'static str;

    /// Called when reference count hits zero.
    /// Default: unimplemented (matches upstream UNIMPLEMENTED()).
    fn destroy(&self) {
        unimplemented!("KAutoObject::Destroy not overridden");
    }

    /// Called to clean up resources without destroying the object.
    fn finalize(&self) {}

    /// Returns the owning process, if any.
    /// Default: None (matches upstream returning nullptr).
    fn get_owner(&self) -> Option<usize> {
        // TODO: Return Option<&KProcess> once KProcess is ported.
        None
    }
}

/// KAutoObject — base kernel object with atomic reference counting.
///
/// Mirrors upstream KAutoObject. The `m_kernel` field is represented as an opaque
/// usize handle until KernelCore is ported.
pub struct KAutoObject {
    /// Reference to the kernel core (opaque handle until KernelCore is ported).
    // TODO: Replace with Arc<KernelCore> when KernelCore is ported.
    pub m_kernel: usize,
    m_ref_count: AtomicU32,
}

impl KAutoObject {
    /// Construct a new KAutoObject. Mirrors `KAutoObject(KernelCore& kernel)`.
    pub fn new(kernel: usize) -> Self {
        // TODO: Call RegisterWithKernel() once KernelCore is ported.
        Self {
            m_kernel: kernel,
            m_ref_count: AtomicU32::new(0),
        }
    }

    /// Mirrors `KAutoObject::Create(KAutoObject* obj)`.
    /// Sets the initial reference count to 1.
    pub fn create_init(&self) {
        self.m_ref_count.store(1, Ordering::Release);
    }

    /// Returns the current reference count.
    pub fn get_reference_count(&self) -> u32 {
        self.m_ref_count.load(Ordering::Acquire)
    }

    /// Check if this object's type is derived from the given TypeObj.
    pub fn is_derived_from_type(&self, rhs: &TypeObj) -> bool
    where
        Self: KAutoObjectBase,
    {
        self.get_type_obj().is_derived_from(rhs)
    }

    /// Atomically increment the reference count, only if it's positive.
    /// Returns true on success, false if the object was already at zero refs.
    /// Mirrors upstream `KAutoObject::Open()`.
    pub fn open(&self) -> bool {
        let mut cur_ref_count = self.m_ref_count.load(Ordering::Acquire);
        loop {
            if cur_ref_count == 0 {
                return false;
            }
            debug_assert!(cur_ref_count < cur_ref_count.wrapping_add(1));
            match self.m_ref_count.compare_exchange_weak(
                cur_ref_count,
                cur_ref_count + 1,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => return true,
                Err(actual) => cur_ref_count = actual,
            }
        }
    }

    /// Atomically decrement the reference count. If it reaches zero, calls destroy.
    /// Mirrors upstream `KAutoObject::Close()`.
    pub fn close(&self)
    where
        Self: KAutoObjectBase,
    {
        let mut cur_ref_count = self.m_ref_count.load(Ordering::Acquire);
        loop {
            debug_assert!(cur_ref_count > 0);
            match self.m_ref_count.compare_exchange_weak(
                cur_ref_count,
                cur_ref_count - 1,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => cur_ref_count = actual,
            }
        }

        if cur_ref_count - 1 == 0 {
            // TODO: let kernel = self.m_kernel;
            self.destroy();
            // TODO: KAutoObject::UnregisterWithKernel(kernel, self);
        }
    }

    // TODO: fn register_with_kernel(&self) — requires KernelCore
    // TODO: fn unregister_with_kernel(kernel: &KernelCore, obj: &KAutoObject) — requires KernelCore
}

impl KAutoObjectBase for KAutoObject {
    fn get_type_obj(&self) -> TypeObj {
        Self::get_static_type_obj()
    }

    fn get_type_name(&self) -> &'static str {
        Self::get_static_type_name()
    }
}

impl KAutoObject {
    pub fn get_static_type_obj() -> TypeObj {
        TypeObj::new("KAutoObject", k_class_token::class_token(k_class_token::ObjectType::KAutoObject))
    }

    pub fn get_static_type_name() -> &'static str {
        "KAutoObject"
    }
}

/// KAutoObjectWithList — extends KAutoObject with an intrusive container hook.
/// Mirrors the C++ `KAutoObjectWithList : public KAutoObject, public boost::intrusive::set_base_hook<>`.
///
/// In Rust, the intrusive set_base_hook is not directly representable.
/// We store a unique ID for ordering (matching upstream's address-based comparison).
pub struct KAutoObjectWithList {
    pub base: KAutoObject,
    /// Unique ID used for ordering in the object list (replaces address-based comparison).
    id: u64,
}

impl KAutoObjectWithList {
    /// Next unique ID counter for ordering.
    fn next_id() -> u64 {
        use std::sync::atomic::AtomicU64;
        static NEXT_ID: AtomicU64 = AtomicU64::new(1);
        NEXT_ID.fetch_add(1, Ordering::Relaxed)
    }

    pub fn new(kernel: usize) -> Self {
        Self {
            base: KAutoObject::new(kernel),
            id: Self::next_id(),
        }
    }

    /// Compare two KAutoObjectWithList instances by their unique ID.
    /// Mirrors upstream address-based comparison.
    pub fn compare(lhs: &KAutoObjectWithList, rhs: &KAutoObjectWithList) -> i32 {
        if lhs.id < rhs.id {
            -1
        } else if lhs.id > rhs.id {
            1
        } else {
            0
        }
    }

    /// Returns the object's unique ID.
    /// Mirrors upstream `GetId()` which returns `reinterpret_cast<u64>(this)`.
    pub fn get_id(&self) -> u64 {
        self.id
    }
}

impl PartialEq for KAutoObjectWithList {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for KAutoObjectWithList {}

impl PartialOrd for KAutoObjectWithList {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for KAutoObjectWithList {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

/// KScopedAutoObject — RAII wrapper that manages KAutoObject reference counting.
///
/// Mirrors the C++ `KScopedAutoObject<T>` template.
/// On construction (from a raw pointer), calls Open().
/// On drop, calls Close().
///
/// Since Rust doesn't have the same pointer semantics, we use Option<*const T>
/// for the inner storage. This is a simplified port — full pointer-based usage
/// requires the concrete KAutoObject types to be ported.
pub struct KScopedAutoObject<T: KAutoObjectBase> {
    m_obj: Option<*mut T>,
}

impl<T: KAutoObjectBase> KScopedAutoObject<T> {
    /// Create an empty scoped auto object.
    pub fn new_null() -> Self {
        Self { m_obj: None }
    }

    /// Returns true if the inner pointer is null.
    pub fn is_null(&self) -> bool {
        self.m_obj.is_none()
    }

    /// Returns true if the inner pointer is not null.
    pub fn is_not_null(&self) -> bool {
        self.m_obj.is_some()
    }

    /// Get the inner pointer (unsafe — caller must ensure validity).
    pub fn get_pointer_unsafe(&self) -> Option<*mut T> {
        self.m_obj
    }

    /// Release the inner pointer without calling Close.
    pub fn release_pointer_unsafe(&mut self) -> Option<*mut T> {
        self.m_obj.take()
    }
}

// Note: Full KScopedAutoObject functionality (Open/Close on construct/drop,
// move semantics with DynamicCast for up/downcasts) requires concrete kernel
// object types. The above is a structural placeholder matching upstream layout.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_obj() {
        let ty = TypeObj::new("TestObj", 0x1234);
        assert_eq!(ty.get_name(), "TestObj");
        assert_eq!(ty.get_class_token(), 0x1234);
    }

    #[test]
    fn test_type_obj_is_derived_from() {
        let base = TypeObj::new("Base", 0b0001);
        let derived = TypeObj::new("Derived", 0b0011);
        assert!(derived.is_derived_from(&base));
        assert!(!base.is_derived_from(&derived));
    }

    #[test]
    fn test_k_auto_object_open_close() {
        let obj = KAutoObject::new(0);
        obj.create_init();
        assert_eq!(obj.get_reference_count(), 1);
        assert!(obj.open());
        assert_eq!(obj.get_reference_count(), 2);
    }

    #[test]
    fn test_k_auto_object_open_fails_at_zero() {
        let obj = KAutoObject::new(0);
        // ref_count is 0 initially
        assert!(!obj.open());
    }

    #[test]
    fn test_k_auto_object_with_list_ordering() {
        let a = KAutoObjectWithList::new(0);
        let b = KAutoObjectWithList::new(0);
        assert_ne!(a.get_id(), b.get_id());
        // First created should have lower ID
        assert!(a < b);
    }
}

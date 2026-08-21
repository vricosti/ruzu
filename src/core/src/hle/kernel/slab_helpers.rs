//! Port of zuyu/src/core/hle/kernel/slab_helpers.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! Helper traits and types for slab-allocated kernel objects.
//! Mirrors the C++ templates:
//! - KSlabAllocated<Derived>
//! - KAutoObjectWithSlabHeap<Derived, Base>
//! - KAutoObjectWithSlabHeapAndContainer<Derived, Base>
//!
//! These are trait-based in Rust rather than CRTP-template-based.
//! Full wiring requires KernelCore and KAutoObjectContainer.

/// Trait for types that can be slab-allocated.
///
/// Mirrors `Kernel::KSlabAllocated<Derived>`.
/// Provides methods to initialize the slab heap, allocate/free objects,
/// and query the slab state.
///
/// In the host-emulated model, slab allocation uses Box<T> instead of
/// kernel-managed slab heaps. The slab resource counts define limits but
/// allocation itself uses the host allocator.
pub trait KSlabAllocated: Sized {
    /// Initialize the slab heap for this type.
    /// In the host-emulated model, this is a no-op since we use Box allocation.
    fn initialize_slab_heap(_kernel: usize, _num_objects: usize) {
        // Host-emulated: no pre-allocated slab region needed.
    }

    /// Allocate an instance from the slab.
    /// Uses Box allocation in the host-emulated model.
    fn slab_allocate(_kernel: usize) -> Option<Box<Self>>
    where
        Self: Default,
    {
        Some(Box::new(Self::default()))
    }

    /// Free an instance back to the slab.
    /// In the host-emulated model, the Box is simply dropped.
    fn slab_free(_kernel: usize, _obj: Box<Self>) {
        // Box is dropped here, freeing host memory.
    }

    /// Get the object size.
    fn slab_object_size() -> usize {
        std::mem::size_of::<Self>()
    }
}

/// Trait for auto-objects with slab heap allocation.
///
/// Mirrors `Kernel::KAutoObjectWithSlabHeap<Derived, Base>`.
/// Provides Destroy() behavior that finalizes and frees via slab,
/// then calls PostDestroy.
///
/// Upstream calls Finalize, frees via slab, then calls PostDestroy.
/// In the host-emulated model, objects are freed when Box is dropped.
pub trait KAutoObjectWithSlabHeap: KSlabAllocated {
    /// Whether this object has been fully initialized.
    /// Default: true (matching upstream).
    fn is_initialized(&self) -> bool {
        true
    }

    /// Argument passed to PostDestroy after the object is freed.
    /// Default: 0 (matching upstream).
    fn get_post_destroy_argument(&self) -> usize {
        0
    }

    /// Called after the object is freed from the slab.
    /// Matches upstream `static void PostDestroy(uintptr_t arg)`.
    fn post_destroy(_arg: usize)
    where
        Self: Sized,
    {
    }

    /// Create an instance from the slab and initialize its reference count.
    fn create(kernel: usize) -> Option<Box<Self>>
    where
        Self: Sized + Default,
    {
        Self::slab_allocate(kernel)
    }
}

/// Trait for auto-objects with slab heap and container registration.
///
/// Mirrors `Kernel::KAutoObjectWithSlabHeapAndContainer<Derived, Base>`.
/// Extends KAutoObjectWithSlabHeap with container registration/unregistration.
///
/// Upstream registers the object with KAutoObjectContainer for enumeration.
/// In the host-emulated model, registration is a no-op.
pub trait KAutoObjectWithSlabHeapAndContainer: KAutoObjectWithSlabHeap {
    /// Register this object with the kernel's object list container.
    fn register(_kernel: usize, _obj: &Self) {
        // Host-emulated: object list container tracking not needed.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestObj {
        _value: u32,
    }

    impl KSlabAllocated for TestObj {}
    impl KAutoObjectWithSlabHeap for TestObj {}

    #[test]
    fn test_slab_object_size() {
        assert_eq!(TestObj::slab_object_size(), std::mem::size_of::<TestObj>());
    }

    #[test]
    fn test_default_is_initialized() {
        let obj = TestObj { _value: 42 };
        assert!(obj.is_initialized());
    }

    #[test]
    fn test_default_post_destroy_argument() {
        let obj = TestObj { _value: 0 };
        assert_eq!(obj.get_post_destroy_argument(), 0);
    }
}

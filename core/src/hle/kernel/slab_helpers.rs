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
/// TODO: Full implementation requires KernelCore::SlabHeap<T>().
pub trait KSlabAllocated: Sized {
    /// Initialize the slab heap for this type.
    fn initialize_slab_heap(kernel: usize, num_objects: usize) {
        // TODO: kernel.SlabHeap::<Self>().Initialize(memory, memory_size);
        let _ = (kernel, num_objects);
    }

    /// Allocate an instance from the slab.
    fn slab_allocate(_kernel: usize) -> Option<Box<Self>> {
        // TODO: kernel.SlabHeap::<Self>().Allocate(kernel)
        None
    }

    /// Free an instance back to the slab.
    fn slab_free(_kernel: usize, _obj: Box<Self>) {
        // TODO: kernel.SlabHeap::<Self>().Free(obj)
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
/// TODO: Full implementation requires KernelCore and KAutoObject integration.
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
    fn create(_kernel: usize) -> Option<Box<Self>>
    where
        Self: Sized,
    {
        // TODO: let obj = Self::slab_allocate(kernel)?;
        // KAutoObject::Create(obj);
        // Some(obj)
        None
    }
}

/// Trait for auto-objects with slab heap and container registration.
///
/// Mirrors `Kernel::KAutoObjectWithSlabHeapAndContainer<Derived, Base>`.
/// Extends KAutoObjectWithSlabHeap with container registration/unregistration.
///
/// TODO: Requires KAutoObjectWithListContainer (KAutoObjectContainer).
pub trait KAutoObjectWithSlabHeapAndContainer: KAutoObjectWithSlabHeap {
    /// Register this object with the kernel's object list container.
    fn register(_kernel: usize, _obj: &Self) {
        // TODO: kernel.ObjectListContainer().Register(obj);
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

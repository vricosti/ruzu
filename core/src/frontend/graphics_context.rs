// SPDX-FileCopyrightText: 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/graphics_context.h
//! Graphics context interface.

/// Represents a drawing context that supports graphics operations.
///
/// Corresponds to upstream `Core::Frontend::GraphicsContext`.
pub trait GraphicsContext {
    /// Inform the driver to swap the front/back buffers and present the current image.
    fn swap_buffers(&mut self) {}

    /// Makes the graphics context current for the caller thread.
    fn make_current(&mut self) {}

    /// Releases the context from the caller thread.
    fn done_current(&mut self) {}

    /// Gets the GPU driver library (used by Android only).
    /// Returns None by default.
    ///
    /// Upstream returns `std::shared_ptr<Common::DynamicLibrary>`.
    /// Android JNI integration is excluded from this port (see CLAUDE.md exceptions),
    /// so the return type is `Option<()>` as a placeholder. No callers exist in
    /// the non-Android frontend.
    fn get_driver_library(&self) -> Option<()> {
        None
    }
}

/// Non-owning handle corresponding to upstream `Core::Frontend::GraphicsContext&`.
///
/// `video_core::gpu_thread` keeps a graphics context reference across the GPU
/// thread lifetime, like upstream `StartThread(..., GraphicsContext& context)`.
/// Rust trait objects are fat pointers, so this wrapper centralizes the
/// lifetime-erasing conversion instead of storing raw `[usize; 2]` words at
/// the call site.
#[derive(Clone, Copy)]
pub struct GraphicsContextHandle {
    ptr: *const (dyn GraphicsContext + 'static),
}

unsafe impl Send for GraphicsContextHandle {}
unsafe impl Sync for GraphicsContextHandle {}

impl GraphicsContextHandle {
    pub fn from_ref(context: &dyn GraphicsContext) -> Self {
        let ptr = context as *const dyn GraphicsContext;
        Self {
            ptr: unsafe {
                std::mem::transmute::<
                    *const dyn GraphicsContext,
                    *const (dyn GraphicsContext + 'static),
                >(ptr)
            },
        }
    }

    /// # Safety
    ///
    /// The caller must ensure the context outlives the handle and is not
    /// mutably accessed elsewhere while this reference is live. This mirrors
    /// upstream's non-owning `GraphicsContext&` contract.
    pub unsafe fn as_mut<'a>(self) -> &'a mut dyn GraphicsContext {
        unsafe { &mut *(self.ptr as *mut (dyn GraphicsContext + 'static)) }
    }
}

/// RAII guard that makes a GraphicsContext current on creation
/// and calls done_current on drop.
///
/// Corresponds to upstream `GraphicsContext::Scoped`.
pub struct ScopedGraphicsContext<'a> {
    context: &'a mut dyn GraphicsContext,
    active: bool,
}

impl<'a> ScopedGraphicsContext<'a> {
    pub fn new(context: &'a mut dyn GraphicsContext) -> Self {
        context.make_current();
        Self {
            context,
            active: true,
        }
    }

    /// In the event that context was destroyed before the Scoped is destroyed,
    /// this provides a mechanism to prevent calling a destroyed object's method.
    ///
    /// Corresponds to upstream `GraphicsContext::Scoped::Cancel`.
    pub fn cancel(&mut self) {
        self.active = false;
    }
}

impl<'a> Drop for ScopedGraphicsContext<'a> {
    fn drop(&mut self) {
        if self.active {
            self.context.done_current();
        }
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_fence_manager.h and gl_fence_manager.cpp
//!
//! OpenGL fence manager — manages GPU synchronization fences.

use std::sync::Arc;

use super::gl_resource_manager::OGLSync;
use crate::fence_manager::FenceBase;

/// An OpenGL sync fence.
///
/// Corresponds to `OpenGL::GLInnerFence`.
pub struct GLInnerFence {
    /// Whether this fence is stubbed (no actual GL sync).
    pub is_stubbed: bool,
    /// GL sync object wrapper.
    sync_object: OGLSync,
}

impl GLInnerFence {
    /// Create a new fence.
    ///
    /// Corresponds to `GLInnerFence::GLInnerFence()`.
    pub fn new(is_stubbed: bool) -> Self {
        Self {
            is_stubbed,
            sync_object: OGLSync::new(),
        }
    }

    /// Queue the fence into the GL command stream.
    ///
    /// Corresponds to `GLInnerFence::Queue()`.
    pub fn queue(&mut self) {
        if self.is_stubbed {
            return;
        }
        assert!(self.sync_object.handle.is_null());
        self.sync_object.create();
    }

    /// Check if the fence has been signaled.
    ///
    /// Corresponds to `GLInnerFence::IsSignaled()`.
    pub fn is_signaled(&self) -> bool {
        if self.is_stubbed {
            return true;
        }
        assert!(!self.sync_object.handle.is_null());
        self.sync_object.is_signaled()
    }

    /// Wait for the fence to be signaled.
    ///
    /// Corresponds to `GLInnerFence::Wait()`.
    pub fn wait(&self) {
        if self.is_stubbed {
            return;
        }
        assert!(!self.sync_object.handle.is_null());
        unsafe {
            gl::ClientWaitSync(self.sync_object.handle, 0, gl::TIMEOUT_IGNORED);
        }
    }
}

/// Shared fence type.
///
/// Corresponds to `OpenGL::Fence = std::shared_ptr<GLInnerFence>`.
pub type Fence = Arc<std::sync::Mutex<GLInnerFence>>;

impl FenceBase for Fence {
    fn is_stubbed(&self) -> bool {
        self.lock().unwrap().is_stubbed
    }

    fn wait_for_fence(&self) {
        self.lock().unwrap().wait();
    }
}

unsafe impl Send for GLInnerFence {}
unsafe impl Sync for GLInnerFence {}

/// OpenGL fence manager.
///
/// Corresponds to `OpenGL::FenceManagerOpenGL`.
pub struct FenceManagerOpenGL {
    #[cfg(test)]
    force_stubbed_fences: bool,
}

impl FenceManagerOpenGL {
    /// Create a new fence manager.
    pub fn new() -> Self {
        Self {
            #[cfg(test)]
            force_stubbed_fences: false,
        }
    }

    #[cfg(test)]
    pub fn new_for_test() -> Self {
        Self {
            force_stubbed_fences: true,
        }
    }

    /// Create a new fence.
    ///
    /// Corresponds to `FenceManagerOpenGL::CreateFence()`.
    pub fn create_fence(&self, is_stubbed: bool) -> Fence {
        Arc::new(std::sync::Mutex::new(GLInnerFence::new(
            is_stubbed || cfg!(test) && self.force_stubbed_for_test(),
        )))
    }

    #[cfg(test)]
    fn force_stubbed_for_test(&self) -> bool {
        self.force_stubbed_fences
    }

    #[cfg(not(test))]
    fn force_stubbed_for_test(&self) -> bool {
        false
    }

    /// Queue a fence.
    ///
    /// Corresponds to `FenceManagerOpenGL::QueueFence()`.
    pub fn queue_fence(&self, fence: &Fence) {
        fence.lock().unwrap().queue();
    }

    /// Check if a fence is signaled.
    ///
    /// Corresponds to `FenceManagerOpenGL::IsFenceSignaled()`.
    pub fn is_fence_signaled(&self, fence: &Fence) -> bool {
        fence.lock().unwrap().is_signaled()
    }

    /// Wait for a fence.
    ///
    /// Corresponds to `FenceManagerOpenGL::WaitFence()`.
    pub fn wait_fence(&self, fence: &Fence) {
        fence.lock().unwrap().wait();
    }
}

#[cfg(test)]
mod tests {
    use super::GLInnerFence;

    #[test]
    fn stubbed_fence_is_immediately_signaled_and_noop() {
        let mut fence = GLInnerFence::new(true);
        fence.queue();
        assert!(fence.is_signaled());
        fence.wait();
    }
}

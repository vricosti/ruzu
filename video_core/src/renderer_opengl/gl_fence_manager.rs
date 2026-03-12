// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_fence_manager.h and gl_fence_manager.cpp
//!
//! OpenGL fence manager — manages GPU synchronization fences.

use std::sync::Arc;

/// An OpenGL sync fence.
///
/// Corresponds to `OpenGL::GLInnerFence`.
pub struct GLInnerFence {
    /// Whether this fence is stubbed (no actual GL sync).
    pub is_stubbed: bool,
    /// GL sync object handle.
    sync_object: gl::types::GLsync,
}

impl GLInnerFence {
    /// Create a new fence.
    ///
    /// Corresponds to `GLInnerFence::GLInnerFence()`.
    pub fn new(is_stubbed: bool) -> Self {
        Self {
            is_stubbed,
            sync_object: std::ptr::null(),
        }
    }

    /// Queue the fence into the GL command stream.
    ///
    /// Corresponds to `GLInnerFence::Queue()`.
    pub fn queue(&mut self) {
        if self.is_stubbed {
            return;
        }
        assert!(self.sync_object.is_null());
        unsafe {
            self.sync_object = gl::FenceSync(gl::SYNC_GPU_COMMANDS_COMPLETE, 0);
        }
    }

    /// Check if the fence has been signaled.
    ///
    /// Corresponds to `GLInnerFence::IsSignaled()`.
    pub fn is_signaled(&self) -> bool {
        if self.is_stubbed {
            return true;
        }
        assert!(!self.sync_object.is_null());
        let mut status: gl::types::GLint = 0;
        unsafe {
            gl::GetSynciv(
                self.sync_object,
                gl::SYNC_STATUS,
                1,
                std::ptr::null_mut(),
                &mut status,
            );
        }
        status as u32 == gl::SIGNALED
    }

    /// Wait for the fence to be signaled.
    ///
    /// Corresponds to `GLInnerFence::Wait()`.
    pub fn wait(&self) {
        if self.is_stubbed {
            return;
        }
        assert!(!self.sync_object.is_null());
        unsafe {
            gl::ClientWaitSync(self.sync_object, 0, gl::TIMEOUT_IGNORED);
        }
    }
}

impl Drop for GLInnerFence {
    fn drop(&mut self) {
        if !self.sync_object.is_null() {
            unsafe {
                gl::DeleteSync(self.sync_object);
            }
        }
    }
}

/// Shared fence type.
///
/// Corresponds to `OpenGL::Fence = std::shared_ptr<GLInnerFence>`.
pub type Fence = Arc<std::sync::Mutex<GLInnerFence>>;

/// OpenGL fence manager.
///
/// Corresponds to `OpenGL::FenceManagerOpenGL`.
pub struct FenceManagerOpenGL;

impl FenceManagerOpenGL {
    /// Create a new fence manager.
    pub fn new() -> Self {
        Self
    }

    /// Create a new fence.
    ///
    /// Corresponds to `FenceManagerOpenGL::CreateFence()`.
    pub fn create_fence(&self, is_stubbed: bool) -> Fence {
        Arc::new(std::sync::Mutex::new(GLInnerFence::new(is_stubbed)))
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

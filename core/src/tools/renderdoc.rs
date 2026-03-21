// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/tools/renderdoc.h and renderdoc.cpp
//! RenderDoc API integration for frame capture.

/// Opaque handle to RenderDoc API function pointers.
///
/// Corresponds to upstream `RENDERDOC_API_1_6_0*`.
/// On Linux, this is obtained via `dlopen`/`dlsym` on `librenderdoc.so`.
/// On Windows, via `GetModuleHandle`/`GetProcAddress` on `renderdoc.dll`.
///
/// Since we cannot use the actual RenderDoc C API headers directly from Rust,
/// we store a raw pointer obtained at runtime. Full integration would require
/// either renderdoc-sys bindings or the `renderdoc-rs` crate.
struct RdocApi {
    /// Raw pointer to RENDERDOC_API_1_6_0 struct, or null.
    _api_ptr: *mut std::ffi::c_void,
}

// SAFETY: The RenderDoc API is designed for single-threaded capture toggle.
// Upstream stores the pointer as a plain member without synchronization.
unsafe impl Send for RdocApi {}
unsafe impl Sync for RdocApi {}

/// RenderDoc API wrapper.
///
/// Corresponds to upstream `Tools::RenderdocAPI`.
pub struct RenderdocApi {
    rdoc_api: Option<RdocApi>,
    is_capturing: bool,
}

impl RenderdocApi {
    /// Create a new RenderDoc API instance.
    ///
    /// Attempts to load the RenderDoc shared library and retrieve the API entry point.
    /// If RenderDoc is not loaded into the process, the API will be unavailable and
    /// `toggle_capture` will be a no-op.
    ///
    /// Corresponds to upstream `RenderdocAPI::RenderdocAPI()`.
    pub fn new() -> Self {
        let api = Self::try_load_api();
        Self {
            rdoc_api: api,
            is_capturing: false,
        }
    }

    /// Toggle frame capture on/off.
    ///
    /// Corresponds to upstream `RenderdocAPI::ToggleCapture()`.
    pub fn toggle_capture(&mut self) {
        if self.rdoc_api.is_none() {
            return;
        }

        if !self.is_capturing {
            // rdoc_api->StartFrameCapture(NULL, NULL);
            log::info!("RenderDoc: StartFrameCapture (stubbed)");
        } else {
            // rdoc_api->EndFrameCapture(NULL, NULL);
            log::info!("RenderDoc: EndFrameCapture (stubbed)");
        }
        self.is_capturing = !self.is_capturing;
    }

    /// Attempt to load the RenderDoc API from the process.
    ///
    /// On Linux: tries `dlopen("librenderdoc.so", RTLD_NOW | RTLD_NOLOAD)`.
    /// On Android: tries `dlopen("libVkLayer_GLES_RenderDoc.so", ...)`.
    /// On Windows: tries `GetModuleHandleA("renderdoc.dll")`.
    ///
    /// Returns `None` if RenderDoc is not loaded.
    fn try_load_api() -> Option<RdocApi> {
        #[cfg(target_os = "linux")]
        {
            // Try to load RenderDoc if it's already in the process.
            // SAFETY: dlopen with RTLD_NOLOAD only checks if already loaded.
            unsafe {
                #[cfg(target_os = "android")]
                const RENDERDOC_LIB: &[u8] = b"libVkLayer_GLES_RenderDoc.so\0";
                #[cfg(not(target_os = "android"))]
                const RENDERDOC_LIB: &[u8] = b"librenderdoc.so\0";

                let handle = libc::dlopen(
                    RENDERDOC_LIB.as_ptr() as *const libc::c_char,
                    libc::RTLD_NOW | libc::RTLD_NOLOAD,
                );
                if handle.is_null() {
                    return None;
                }

                let get_api_sym = libc::dlsym(
                    handle,
                    b"RENDERDOC_GetAPI\0".as_ptr() as *const libc::c_char,
                );
                if get_api_sym.is_null() {
                    return None;
                }

                // Call RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_6_0, &api_ptr).
                // The function signature is: int RENDERDOC_GetAPI(int version, void** api_ptr)
                // eRENDERDOC_API_Version_1_6_0 = 10600
                type GetApiFn = unsafe extern "C" fn(version: i32, api_ptr: *mut *mut std::ffi::c_void) -> i32;
                let get_api: GetApiFn = std::mem::transmute(get_api_sym);
                let mut api_ptr: *mut std::ffi::c_void = std::ptr::null_mut();
                let ret = get_api(10600, &mut api_ptr);
                if ret != 1 || api_ptr.is_null() {
                    log::warn!("RENDERDOC_GetAPI failed (ret={})", ret);
                    return None;
                }
                log::info!("RenderDoc API loaded successfully");
                Some(RdocApi {
                    _api_ptr: api_ptr,
                })
            }
        }

        #[cfg(not(target_os = "linux"))]
        {
            // Windows: would use GetModuleHandleA("renderdoc.dll") + GetProcAddress.
            // Not implemented — RenderDoc integration is Linux-only for now.
            None
        }
    }
}

impl Default for RenderdocApi {
    fn default() -> Self {
        Self::new()
    }
}

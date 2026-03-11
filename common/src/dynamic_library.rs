// SPDX-FileCopyrightText: 2019 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/dynamic_library.h and zuyu/src/common/dynamic_library.cpp
//!
//! Provides a platform-independent interface for loading a dynamic library
//! and retrieving symbols. Uses `dlopen`/`dlsym` on Unix and
//! `LoadLibraryA`/`GetProcAddress` on Windows.

use std::ffi::{CStr, CString};
use std::os::raw::c_void;

/// Provides a platform-independent interface for loading a dynamic library
/// and retrieving symbols.
///
/// Corresponds to `Common::DynamicLibrary` in C++.
pub struct DynamicLibrary {
    /// Platform-dependent handle representing the loaded library.
    handle: *mut c_void,
}

// Safety: The handle is a raw pointer to a platform library handle.
// DynamicLibrary manages its lifetime and does not allow shared access
// to the handle pointer itself.
unsafe impl Send for DynamicLibrary {}

impl DynamicLibrary {
    /// Default constructor, does not load a library.
    /// Corresponds to `DynamicLibrary::DynamicLibrary()`.
    pub fn new() -> Self {
        Self {
            handle: std::ptr::null_mut(),
        }
    }

    /// Automatically loads the specified library.
    /// Call `is_open()` to check validity before use.
    /// Corresponds to `DynamicLibrary::DynamicLibrary(const char* filename)`.
    pub fn from_filename(filename: &str) -> Self {
        let mut lib = Self::new();
        let _ = lib.open(filename);
        lib
    }

    /// Initializes the dynamic library with an already opened handle.
    /// Corresponds to `DynamicLibrary::DynamicLibrary(void* handle_)`.
    ///
    /// # Safety
    /// The caller must ensure that `handle` is a valid library handle
    /// obtained from `dlopen` (Unix) or `LoadLibraryA` (Windows),
    /// or null.
    pub unsafe fn from_handle(handle: *mut c_void) -> Self {
        Self { handle }
    }

    /// Returns the specified library name with the platform-specific suffix added.
    /// Corresponds to `DynamicLibrary::GetUnprefixedFilename`.
    pub fn get_unprefixed_filename(filename: &str) -> String {
        #[cfg(target_os = "windows")]
        {
            format!("{}.dll", filename)
        }
        #[cfg(target_os = "macos")]
        {
            format!("{}.dylib", filename)
        }
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        {
            format!("{}.so", filename)
        }
    }

    /// Returns the specified library name in platform-specific format.
    /// Major/minor versions will not be included if set to -1 (None).
    /// If libname already contains the "lib" prefix, it will not be added again.
    ///
    /// Windows: LIBNAME-MAJOR-MINOR.dll
    /// Linux: libLIBNAME.so.MAJOR.MINOR
    /// Mac: libLIBNAME.MAJOR.MINOR.dylib
    ///
    /// Corresponds to `DynamicLibrary::GetVersionedFilename`.
    pub fn get_versioned_filename(libname: &str, major: Option<i32>, minor: Option<i32>) -> String {
        #[cfg(target_os = "windows")]
        {
            match (major, minor) {
                (Some(maj), Some(min)) if maj >= 0 && min >= 0 => {
                    format!("{}-{}-{}.dll", libname, maj, min)
                }
                (Some(maj), _) if maj >= 0 => {
                    format!("{}-{}.dll", libname, maj)
                }
                _ => {
                    format!("{}.dll", libname)
                }
            }
        }
        #[cfg(target_os = "macos")]
        {
            let prefix = if libname.starts_with("lib") {
                ""
            } else {
                "lib"
            };
            match (major, minor) {
                (Some(maj), Some(min)) if maj >= 0 && min >= 0 => {
                    format!("{}{}.{}.{}.dylib", prefix, libname, maj, min)
                }
                (Some(maj), _) if maj >= 0 => {
                    format!("{}{}.{}.dylib", prefix, libname, maj)
                }
                _ => {
                    format!("{}{}.dylib", prefix, libname)
                }
            }
        }
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        {
            let prefix = if libname.starts_with("lib") {
                ""
            } else {
                "lib"
            };
            match (major, minor) {
                (Some(maj), Some(min)) if maj >= 0 && min >= 0 => {
                    format!("{}{}.so.{}.{}", prefix, libname, maj, min)
                }
                (Some(maj), _) if maj >= 0 => {
                    format!("{}{}.so.{}", prefix, libname, maj)
                }
                _ => {
                    format!("{}{}.so", prefix, libname)
                }
            }
        }
    }

    /// Returns true if a module is loaded, otherwise false.
    /// Corresponds to `DynamicLibrary::IsOpen()`.
    #[inline]
    pub fn is_open(&self) -> bool {
        !self.handle.is_null()
    }

    /// Loads (or replaces) the handle with the specified library file name.
    /// Returns true if the library was loaded and can be used.
    /// Corresponds to `DynamicLibrary::Open`.
    pub fn open(&mut self, filename: &str) -> bool {
        // Close any existing library first.
        self.close();

        let c_filename = match CString::new(filename) {
            Ok(s) => s,
            Err(_) => return false,
        };

        #[cfg(unix)]
        {
            self.handle = unsafe { libc::dlopen(c_filename.as_ptr(), libc::RTLD_NOW) };
        }

        #[cfg(windows)]
        {
            self.handle = unsafe {
                winapi::um::libloaderapi::LoadLibraryA(c_filename.as_ptr()) as *mut c_void
            };
        }

        self.is_open()
    }

    /// Unloads the library, any function pointers from this library are no longer valid.
    /// Corresponds to `DynamicLibrary::Close`.
    pub fn close(&mut self) {
        if !self.is_open() {
            return;
        }

        #[cfg(unix)]
        {
            unsafe {
                libc::dlclose(self.handle);
            }
        }

        #[cfg(windows)]
        {
            unsafe {
                winapi::um::libloaderapi::FreeLibrary(
                    self.handle as winapi::shared::minwindef::HMODULE,
                );
            }
        }

        self.handle = std::ptr::null_mut();
    }

    /// Returns the address of the specified symbol (function or variable) as an untyped pointer.
    /// If the specified symbol does not exist in this library, returns null.
    /// Corresponds to `DynamicLibrary::GetSymbolAddress`.
    pub fn get_symbol_address(&self, name: &str) -> *mut c_void {
        let c_name = match CString::new(name) {
            Ok(s) => s,
            Err(_) => return std::ptr::null_mut(),
        };

        #[cfg(unix)]
        {
            unsafe { libc::dlsym(self.handle, c_name.as_ptr()) }
        }

        #[cfg(windows)]
        {
            unsafe {
                winapi::um::libloaderapi::GetProcAddress(
                    self.handle as winapi::shared::minwindef::HMODULE,
                    c_name.as_ptr(),
                ) as *mut c_void
            }
        }
    }

    /// Obtains the address of the specified symbol, automatically casting to the correct type.
    /// Returns Some(symbol) if found, None otherwise.
    /// Corresponds to the template `DynamicLibrary::GetSymbol`.
    ///
    /// # Safety
    /// The caller must ensure that `T` matches the actual type of the symbol.
    pub unsafe fn get_symbol<T>(&self, name: &str) -> Option<T>
    where
        T: Copy,
    {
        let addr = self.get_symbol_address(name);
        if addr.is_null() {
            None
        } else {
            Some(std::ptr::read(&addr as *const *mut c_void as *const T))
        }
    }

    /// Returns the raw handle. Useful for passing to other C APIs.
    pub fn handle(&self) -> *mut c_void {
        self.handle
    }
}

impl Default for DynamicLibrary {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for DynamicLibrary {
    /// Closes the library when dropped.
    /// Corresponds to `DynamicLibrary::~DynamicLibrary()`.
    fn drop(&mut self) {
        self.close();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_not_open() {
        let lib = DynamicLibrary::new();
        assert!(!lib.is_open());
    }

    #[test]
    fn test_get_unprefixed_filename() {
        let name = DynamicLibrary::get_unprefixed_filename("mylib");
        #[cfg(target_os = "linux")]
        assert_eq!(name, "mylib.so");
        #[cfg(target_os = "macos")]
        assert_eq!(name, "mylib.dylib");
        #[cfg(target_os = "windows")]
        assert_eq!(name, "mylib.dll");
    }

    #[test]
    fn test_get_versioned_filename_linux() {
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        {
            assert_eq!(
                DynamicLibrary::get_versioned_filename("vulkan", Some(1), Some(0)),
                "libvulkan.so.1.0"
            );
            assert_eq!(
                DynamicLibrary::get_versioned_filename("vulkan", Some(1), None),
                "libvulkan.so.1"
            );
            assert_eq!(
                DynamicLibrary::get_versioned_filename("vulkan", None, None),
                "libvulkan.so"
            );
            // Already has lib prefix
            assert_eq!(
                DynamicLibrary::get_versioned_filename("libvulkan", Some(1), None),
                "libvulkan.so.1"
            );
        }
    }

    #[test]
    fn test_open_nonexistent() {
        let mut lib = DynamicLibrary::new();
        let result = lib.open("nonexistent_library_12345.so");
        assert!(!result);
        assert!(!lib.is_open());
    }

    #[test]
    fn test_close_when_not_open() {
        let mut lib = DynamicLibrary::new();
        lib.close(); // Should not panic
        assert!(!lib.is_open());
    }

    #[test]
    fn test_get_symbol_when_not_open() {
        let lib = DynamicLibrary::new();
        let addr = lib.get_symbol_address("some_symbol");
        assert!(addr.is_null());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_open_libc() {
        let mut lib = DynamicLibrary::new();
        // libc.so.6 should be available on most Linux systems
        if lib.open("libc.so.6") {
            assert!(lib.is_open());
            let addr = lib.get_symbol_address("printf");
            assert!(!addr.is_null());
            lib.close();
            assert!(!lib.is_open());
        }
        // If libc.so.6 isn't available, the test still passes
    }
}

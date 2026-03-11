// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/cmif_serialization.h
//! Status: Structural stub
//!
//! The upstream file contains heavily template-metaprogrammed CMIF serialization/deserialization
//! for automatic argument marshalling of IPC service methods. This includes:
//! - ArgumentType enum for classifying IPC arguments
//! - RequestLayout struct for computing IPC buffer layouts
//! - GetInRawDataSize / GetOutRawDataSize: compile-time size computations
//! - ReadInArgument / WriteOutArgument: recursive template argument (de)serialization
//! - CmifReplyWrapImpl: the main wrapper that reads inputs, calls the handler, writes outputs
//!
//! In Rust, this compile-time type-level dispatch is not directly translatable using the same
//! pattern. Instead, services will use explicit RequestParser/ResponseBuilder calls (matching
//! the non-CMIF path upstream). The CMIF serialization layer can be implemented later using
//! proc macros if needed.

/// Argument type classification, matching upstream `ArgumentType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgumentType {
    InProcessId,
    InData,
    InInterface,
    InCopyHandle,
    OutData,
    OutInterface,
    OutCopyHandle,
    OutMoveHandle,
    InBuffer,
    InLargeData,
    OutBuffer,
    OutLargeData,
}

/// Request layout descriptor, matching upstream `RequestLayout` struct.
#[derive(Debug, Clone, Copy, Default)]
pub struct RequestLayout {
    pub copy_handle_count: u32,
    pub move_handle_count: u32,
    pub cmif_raw_data_size: u32,
    pub domain_interface_count: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_request_layout_default() {
        let layout = RequestLayout::default();
        assert_eq!(layout.copy_handle_count, 0);
        assert_eq!(layout.move_handle_count, 0);
        assert_eq!(layout.cmif_raw_data_size, 0);
        assert_eq!(layout.domain_interface_count, 0);
    }
}

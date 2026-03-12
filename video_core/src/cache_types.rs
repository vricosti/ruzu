// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/cache_types.h

use bitflags::bitflags;

bitflags! {
    /// Types of caches that can be invalidated or flushed.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct CacheType: u32 {
        const NONE = 0;
        const TEXTURE_CACHE = 1 << 0;
        const QUERY_CACHE = 1 << 1;
        const BUFFER_CACHE = 1 << 2;
        const SHADER_CACHE = 1 << 3;
        const NO_TEXTURE_CACHE = Self::QUERY_CACHE.bits() | Self::BUFFER_CACHE.bits() | Self::SHADER_CACHE.bits();
        const NO_BUFFER_CACHE = Self::TEXTURE_CACHE.bits() | Self::QUERY_CACHE.bits() | Self::SHADER_CACHE.bits();
        const NO_QUERY_CACHE = Self::TEXTURE_CACHE.bits() | Self::BUFFER_CACHE.bits() | Self::SHADER_CACHE.bits();
        const ALL = Self::TEXTURE_CACHE.bits() | Self::QUERY_CACHE.bits() | Self::BUFFER_CACHE.bits() | Self::SHADER_CACHE.bits();
    }
}

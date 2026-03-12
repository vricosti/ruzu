// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/samples_helper.h
//!
//! Helpers for converting between MSAA sample counts and log2 representations,
//! and for mapping MsaaMode enumeration values to concrete sample counts.

// ── MsaaMode ───────────────────────────────────────────────────────────
// Upstream lives in video_core/textures/texture.h (Tegra::Texture::MsaaMode).
// Reproduced here to avoid a circular dependency until the textures crate is
// ported.  Must stay in sync with upstream enum values.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum MsaaMode {
    Msaa1x1 = 0,
    Msaa2x1 = 1,
    Msaa2x2 = 2,
    Msaa4x2 = 3,
    Msaa4x4 = 4,
    Msaa2x1D3D = 5,
    Msaa2x2Vc4 = 6,
    Msaa2x2Vc12 = 7,
    Msaa4x2D3D = 8,
    Msaa4x2Vc8 = 9,
    Msaa4x2Vc24 = 10,
}

// ── Public helpers ─────────────────────────────────────────────────────

/// Returns (log2_x, log2_y) for a given sample count.
///
/// Port of `SamplesLog2` from samples_helper.h.
pub fn samples_log2(num_samples: i32) -> (i32, i32) {
    match num_samples {
        1 => (0, 0),
        2 => (1, 0),
        4 => (1, 1),
        8 => (2, 1),
        16 => (2, 2),
        _ => {
            debug_assert!(false, "Invalid number of samples={}", num_samples);
            (0, 0)
        }
    }
}

/// Returns the total number of samples for a given MSAA mode.
///
/// Port of `NumSamples` from samples_helper.h.
pub fn num_samples(msaa_mode: MsaaMode) -> i32 {
    match msaa_mode {
        MsaaMode::Msaa1x1 => 1,
        MsaaMode::Msaa2x1 | MsaaMode::Msaa2x1D3D => 2,
        MsaaMode::Msaa2x2 | MsaaMode::Msaa2x2Vc4 | MsaaMode::Msaa2x2Vc12 => 4,
        MsaaMode::Msaa4x2 | MsaaMode::Msaa4x2D3D | MsaaMode::Msaa4x2Vc8 | MsaaMode::Msaa4x2Vc24 => {
            8
        }
        MsaaMode::Msaa4x4 => 16,
    }
}

/// Returns the horizontal sample count for a given MSAA mode.
///
/// Port of `NumSamplesX` from samples_helper.h.
pub fn num_samples_x(msaa_mode: MsaaMode) -> i32 {
    match msaa_mode {
        MsaaMode::Msaa1x1 => 1,
        MsaaMode::Msaa2x1
        | MsaaMode::Msaa2x1D3D
        | MsaaMode::Msaa2x2
        | MsaaMode::Msaa2x2Vc4
        | MsaaMode::Msaa2x2Vc12 => 2,
        MsaaMode::Msaa4x2
        | MsaaMode::Msaa4x2D3D
        | MsaaMode::Msaa4x2Vc8
        | MsaaMode::Msaa4x2Vc24
        | MsaaMode::Msaa4x4 => 4,
    }
}

/// Returns the vertical sample count for a given MSAA mode.
///
/// Port of `NumSamplesY` from samples_helper.h.
pub fn num_samples_y(msaa_mode: MsaaMode) -> i32 {
    match msaa_mode {
        MsaaMode::Msaa1x1 | MsaaMode::Msaa2x1 | MsaaMode::Msaa2x1D3D => 1,
        MsaaMode::Msaa2x2
        | MsaaMode::Msaa2x2Vc4
        | MsaaMode::Msaa2x2Vc12
        | MsaaMode::Msaa4x2
        | MsaaMode::Msaa4x2D3D
        | MsaaMode::Msaa4x2Vc8
        | MsaaMode::Msaa4x2Vc24 => 2,
        MsaaMode::Msaa4x4 => 4,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_samples_log2() {
        assert_eq!(samples_log2(1), (0, 0));
        assert_eq!(samples_log2(2), (1, 0));
        assert_eq!(samples_log2(4), (1, 1));
        assert_eq!(samples_log2(8), (2, 1));
        assert_eq!(samples_log2(16), (2, 2));
    }

    #[test]
    fn test_num_samples() {
        assert_eq!(num_samples(MsaaMode::Msaa1x1), 1);
        assert_eq!(num_samples(MsaaMode::Msaa2x1), 2);
        assert_eq!(num_samples(MsaaMode::Msaa2x2), 4);
        assert_eq!(num_samples(MsaaMode::Msaa4x2), 8);
        assert_eq!(num_samples(MsaaMode::Msaa4x4), 16);
    }

    #[test]
    fn test_num_samples_xy() {
        assert_eq!(num_samples_x(MsaaMode::Msaa4x2), 4);
        assert_eq!(num_samples_y(MsaaMode::Msaa4x2), 2);
        assert_eq!(num_samples_x(MsaaMode::Msaa1x1), 1);
        assert_eq!(num_samples_y(MsaaMode::Msaa1x1), 1);
    }
}

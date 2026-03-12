// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/transform_feedback.h and video_core/transform_feedback.cpp
//!
//! Transform feedback state and varying generation.

/// Number of transform feedback buffers (matches Maxwell3D::Regs::NumTransformFeedbackBuffers).
pub const NUM_TRANSFORM_FEEDBACK_BUFFERS: usize = 4;

/// Layout for a single transform feedback buffer.
#[derive(Debug, Clone, Copy, Default)]
pub struct TransformFeedbackLayout {
    pub stream: u32,
    pub varying_count: u32,
    pub stride: u32,
}

/// Stream-out layout for a single varying component.
#[derive(Debug, Clone, Copy, Default)]
pub struct StreamOutLayout {
    raw: u32,
}

impl StreamOutLayout {
    pub fn attribute0(&self) -> u32 {
        self.raw & 0xFF
    }
    pub fn attribute1(&self) -> u32 {
        (self.raw >> 8) & 0xFF
    }
    pub fn attribute2(&self) -> u32 {
        (self.raw >> 16) & 0xFF
    }
    pub fn attribute3(&self) -> u32 {
        (self.raw >> 24) & 0xFF
    }
}

/// Complete transform feedback state.
#[derive(Debug, Clone)]
pub struct TransformFeedbackState {
    pub layouts: [TransformFeedbackLayout; NUM_TRANSFORM_FEEDBACK_BUFFERS],
    pub varyings: [[StreamOutLayout; 32]; NUM_TRANSFORM_FEEDBACK_BUFFERS],
}

impl Default for TransformFeedbackState {
    fn default() -> Self {
        Self {
            layouts: [TransformFeedbackLayout::default(); NUM_TRANSFORM_FEEDBACK_BUFFERS],
            varyings: [[StreamOutLayout::default(); 32]; NUM_TRANSFORM_FEEDBACK_BUFFERS],
        }
    }
}

/// A single transform feedback varying descriptor.
#[derive(Debug, Clone, Copy, Default)]
pub struct TransformFeedbackVarying {
    pub buffer: u32,
    pub stride: u32,
    pub offset: u32,
    pub components: u32,
}

/// Vector attribute base offsets used for transform feedback varying mapping.
const VECTORS: [u32; 44] = [
    28,  // gl_Position
    32,  // Generic 0
    36,  // Generic 1
    40,  // Generic 2
    44,  // Generic 3
    48,  // Generic 4
    52,  // Generic 5
    56,  // Generic 6
    60,  // Generic 7
    64,  // Generic 8
    68,  // Generic 9
    72,  // Generic 10
    76,  // Generic 11
    80,  // Generic 12
    84,  // Generic 13
    88,  // Generic 14
    92,  // Generic 15
    96,  // Generic 16
    100, // Generic 17
    104, // Generic 18
    108, // Generic 19
    112, // Generic 20
    116, // Generic 21
    120, // Generic 22
    124, // Generic 23
    128, // Generic 24
    132, // Generic 25
    136, // Generic 26
    140, // Generic 27
    144, // Generic 28
    148, // Generic 29
    152, // Generic 30
    156, // Generic 31
    160, // gl_FrontColor
    164, // gl_FrontSecondaryColor
    160, // gl_BackColor
    164, // gl_BackSecondaryColor
    192, // gl_TexCoord[0]
    196, // gl_TexCoord[1]
    200, // gl_TexCoord[2]
    204, // gl_TexCoord[3]
    208, // gl_TexCoord[4]
    212, // gl_TexCoord[5]
    216, // gl_TexCoord[6]
    // 220, // gl_TexCoord[7] -- not included since array is 44
];

/// Generate transform feedback varyings from the given state.
///
/// Returns the varying array and the count of used entries.
pub fn make_transform_feedback_varyings(
    state: &TransformFeedbackState,
) -> ([TransformFeedbackVarying; 256], u32) {
    let mut xfb = [TransformFeedbackVarying::default(); 256];
    let mut count = 0u32;

    for buffer in 0..state.layouts.len() {
        let locations = &state.varyings[buffer];
        let layout = &state.layouts[buffer];
        let varying_count = layout.varying_count;
        let mut highest = 0u32;
        let mut offset = 0u32;

        while offset < varying_count {
            let get_attribute = |index: u32| -> u32 {
                let loc = &locations[(index / 4) as usize];
                match index % 4 {
                    0 => loc.attribute0(),
                    1 => loc.attribute1(),
                    2 => loc.attribute2(),
                    3 => loc.attribute3(),
                    _ => unreachable!(),
                }
            };

            if layout.stream != 0 {
                log::warn!("Stream is not zero: {}", layout.stream);
            }

            let mut varying = TransformFeedbackVarying {
                buffer: buffer as u32,
                stride: layout.stride,
                offset: offset * 4,
                components: 1,
            };

            let base_offset = offset;
            let attribute = get_attribute(offset);

            // Check if this attribute is aligned to a 4-component vector
            let aligned_attr = attribute & !3;
            if VECTORS.contains(&aligned_attr) {
                if attribute % 4 != 0 {
                    log::warn!("Unaligned TFB {}", attribute);
                }
                let base_index = attribute / 4;
                while offset + 1 < varying_count && base_index == get_attribute(offset + 1) / 4 {
                    offset += 1;
                    varying.components += 1;
                }
            }

            if (attribute as usize) < xfb.len() {
                xfb[attribute as usize] = varying;
                count = count.max(attribute);
            }
            highest = highest.max((base_offset + varying.components) * 4);
            offset += 1;
        }

        if highest != layout.stride {
            log::warn!(
                "Transform feedback highest {} != stride {}",
                highest,
                layout.stride
            );
        }
    }

    (xfb, count + 1)
}

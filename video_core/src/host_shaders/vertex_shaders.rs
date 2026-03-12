// Embedded vertex shaders (.vert) from upstream host_shaders/
// Maps to: /home/vricosti/shared/zuyu/src/video_core/host_shaders/

/// Upstream: `host_shaders/full_screen_triangle.vert`
pub const FULL_SCREEN_TRIANGLE_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

#ifdef VULKAN
#define VERTEX_ID gl_VertexIndex
#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define FLIPY 1
#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv
#define VERTEX_ID gl_VertexID
#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define FLIPY -1
#define UNIFORM(n) layout (location = n) uniform
out gl_PerVertex {
    vec4 gl_Position;
};
#endif

BEGIN_PUSH_CONSTANTS
UNIFORM(0) vec2 tex_scale;
UNIFORM(1) vec2 tex_offset;
END_PUSH_CONSTANTS

layout(location = 0) out vec2 texcoord;

void main() {
    float x = float((VERTEX_ID & 1) << 2);
    float y = float((VERTEX_ID & 2) << 1);
    gl_Position = vec4(x - 1.0, FLIPY * (y - 1.0), 0.0, 1.0);
    texcoord = fma(vec2(x, y) / 2.0, tex_scale, tex_offset);
}
"#;

/// Upstream: `host_shaders/fxaa.vert`
pub const FXAA_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

out gl_PerVertex {
    vec4 gl_Position;
};

const vec2 vertices[3] =
    vec2[3](vec2(-1,-1), vec2(3,-1), vec2(-1, 3));

layout (location = 0) out vec4 posPos;

#ifdef VULKAN

#define BINDING_COLOR_TEXTURE 0
#define VERTEX_ID gl_VertexIndex

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#define BINDING_COLOR_TEXTURE 0
#define VERTEX_ID gl_VertexID

#endif

layout (binding = BINDING_COLOR_TEXTURE) uniform sampler2D input_texture;

const float FXAA_SUBPIX_SHIFT = 0;

void main() {
  vec2 vertex = vertices[VERTEX_ID];
  gl_Position = vec4(vertex, 0.0, 1.0);
  vec2 vert_tex_coord = (vertex + 1.0) / 2.0;
  posPos.xy = vert_tex_coord;
  posPos.zw = vert_tex_coord - (0.5 + FXAA_SUBPIX_SHIFT) / textureSize(input_texture, 0);
}
"#;

/// Upstream: `host_shaders/opengl_present.vert`
pub const OPENGL_PRESENT_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430 core

out gl_PerVertex {
    vec4 gl_Position;
};

layout (location = 0) in vec2 vert_position;
layout (location = 1) in vec2 vert_tex_coord;
layout (location = 0) out vec2 frag_tex_coord;

// This is a truncated 3x3 matrix for 2D transformations:
// The upper-left 2x2 submatrix performs scaling/rotation/mirroring.
// The third column performs translation.
// The third row could be used for projection, which we don't need in 2D. It hence is assumed to
// implicitly be [0, 0, 1]
layout (location = 0) uniform mat3x2 modelview_matrix;

void main() {
    // Multiply input position by the rotscale part of the matrix and then manually translate by
    // the last column. This is equivalent to using a full 3x3 matrix and expanding the vector
    // to `vec3(vert_position.xy, 1.0)`
    gl_Position = vec4(mat2(modelview_matrix) * vert_position + modelview_matrix[2], 0.0, 1.0);
    frag_tex_coord = vert_tex_coord;
}
"#;

/// Upstream: `host_shaders/smaa_blending_weight_calculation.vert`
pub const SMAA_BLENDING_WEIGHT_CALCULATION_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

#ifdef VULKAN
#define VERTEX_ID gl_VertexIndex
#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv
#define VERTEX_ID gl_VertexID
#endif

out gl_PerVertex {
    vec4 gl_Position;
};

const vec2 vertices[3] =
    vec2[3](vec2(-1,-1), vec2(3,-1), vec2(-1, 3));

layout (binding = 0) uniform sampler2D edges_tex;
layout (binding = 1) uniform sampler2D area_tex;
layout (binding = 2) uniform sampler2D search_tex;

layout (location = 0) out vec2 tex_coord;
layout (location = 1) out vec2 pix_coord;
layout (location = 2) out vec4 offset[3];

vec4 metrics = vec4(1.0 / textureSize(edges_tex, 0), textureSize(edges_tex, 0));
#define SMAA_RT_METRICS metrics
#define SMAA_GLSL_4
#define SMAA_PRESET_ULTRA
#define SMAA_INCLUDE_VS 1
#define SMAA_INCLUDE_PS 0

#include "opengl_smaa.glsl"

void main() {
    vec2 vertex = vertices[VERTEX_ID];
    gl_Position = vec4(vertex, 0.0, 1.0);
    tex_coord = (vertex + 1.0) / 2.0;
    SMAABlendingWeightCalculationVS(tex_coord, pix_coord, offset);
}
"#;

/// Upstream: `host_shaders/smaa_edge_detection.vert`
pub const SMAA_EDGE_DETECTION_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

#ifdef VULKAN
#define VERTEX_ID gl_VertexIndex
#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv
#define VERTEX_ID gl_VertexID
#endif

out gl_PerVertex {
    vec4 gl_Position;
};

const vec2 vertices[3] =
    vec2[3](vec2(-1,-1), vec2(3,-1), vec2(-1, 3));

layout (binding = 0) uniform sampler2D input_tex;

layout (location = 0) out vec2 tex_coord;
layout (location = 1) out vec4 offset[3];

vec4 metrics = vec4(1.0 / textureSize(input_tex, 0), textureSize(input_tex, 0));
#define SMAA_RT_METRICS metrics
#define SMAA_GLSL_4
#define SMAA_PRESET_ULTRA
#define SMAA_INCLUDE_VS 1
#define SMAA_INCLUDE_PS 0

#include "opengl_smaa.glsl"

void main() {
    vec2 vertex = vertices[VERTEX_ID];
    gl_Position = vec4(vertex, 0.0, 1.0);
    tex_coord = (vertex + 1.0) / 2.0;
    SMAAEdgeDetectionVS(tex_coord, offset);
}
"#;

/// Upstream: `host_shaders/smaa_neighborhood_blending.vert`
pub const SMAA_NEIGHBORHOOD_BLENDING_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

#ifdef VULKAN
#define VERTEX_ID gl_VertexIndex
#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv
#define VERTEX_ID gl_VertexID
#endif

out gl_PerVertex {
    vec4 gl_Position;
};

const vec2 vertices[3] =
    vec2[3](vec2(-1,-1), vec2(3,-1), vec2(-1, 3));

layout (binding = 0) uniform sampler2D input_tex;
layout (binding = 1) uniform sampler2D blend_tex;

layout (location = 0) out vec2 tex_coord;
layout (location = 1) out vec4 offset;

vec4 metrics = vec4(1.0 / textureSize(input_tex, 0), textureSize(input_tex, 0));
#define SMAA_RT_METRICS metrics
#define SMAA_GLSL_4
#define SMAA_PRESET_ULTRA
#define SMAA_INCLUDE_VS 1
#define SMAA_INCLUDE_PS 0

#include "opengl_smaa.glsl"

void main() {
    vec2 vertex = vertices[VERTEX_ID];
    gl_Position = vec4(vertex, 0.0, 1.0);
    tex_coord = (vertex + 1.0) / 2.0;
    SMAANeighborhoodBlendingVS(tex_coord, offset);
}
"#;

/// Upstream: `host_shaders/vulkan_color_clear.vert`
pub const VULKAN_COLOR_CLEAR_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

void main() {
    float x = float((gl_VertexIndex & 1) << 2);
    float y = float((gl_VertexIndex & 2) << 1);
    gl_Position = vec4(x - 1.0, y - 1.0, 0.0, 1.0);
}
"#;

/// Upstream: `host_shaders/vulkan_fidelityfx_fsr.vert`
pub const VULKAN_FIDELITYFX_FSR_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

layout(location = 0) out vec2 texcoord;

void main() {
    float x = float((gl_VertexIndex & 1) << 2);
    float y = float((gl_VertexIndex & 2) << 1);
    gl_Position = vec4(x - 1.0, y - 1.0, 0.0, 1.0);
    texcoord = vec2(x, y) / 2.0;
}
"#;

/// Upstream: `host_shaders/vulkan_present.vert`
pub const VULKAN_PRESENT_VERT: &str = r#"// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

layout (location = 0) out vec2 frag_tex_coord;

struct ScreenRectVertex {
    vec2 position;
    vec2 tex_coord;
};

layout (push_constant) uniform PushConstants {
    mat4 modelview_matrix;
    ScreenRectVertex vertices[4];
};

// Vulkan spec 15.8.1:
//   Any member of a push constant block that is declared as an
//   array must only be accessed with dynamically uniform indices.
ScreenRectVertex GetVertex(int index) {
    if (index < 1) {
        return vertices[0];
    } else if (index < 2) {
        return vertices[1];
    } else if (index < 3) {
        return vertices[2];
    } else {
        return vertices[3];
    }
}

void main() {
    ScreenRectVertex vertex = GetVertex(gl_VertexIndex);
    gl_Position = modelview_matrix * vec4(vertex.position, 0.0, 1.0);
    frag_tex_coord = vertex.tex_coord;
}
"#;

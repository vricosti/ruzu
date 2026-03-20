// Embedded fragment shaders (.frag) from upstream host_shaders/
// Maps to: /home/vricosti/shared/zuyu/src/video_core/host_shaders/

/// Upstream: `host_shaders/blit_color_float.frag`
pub const BLIT_COLOR_FLOAT_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

layout(binding = 0) uniform sampler2D tex;

layout(location = 0) in vec2 texcoord;
layout(location = 0) out vec4 color;

void main() {
    color = textureLod(tex, texcoord, 0);
}
"#;

/// Upstream: `host_shaders/convert_abgr8_to_d24s8.frag`
pub const CONVERT_ABGR8_TO_D24S8_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450
#extension GL_ARB_shader_stencil_export : require

layout(binding = 0) uniform sampler2D color_texture;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    uvec4 color = uvec4(texelFetch(color_texture, coord, 0).abgr * (exp2(8) - 1.0f));
    uvec4 bytes = color << uvec4(24, 16, 8, 0);
    uint depth_stencil_unorm = bytes.x | bytes.y | bytes.z | bytes.w;

    gl_FragDepth = float(depth_stencil_unorm & 0x00FFFFFFu) / (exp2(24.0) - 1.0f);
    gl_FragStencilRefARB = int(depth_stencil_unorm >> 24);
}
"#;

/// Upstream: `host_shaders/convert_abgr8_to_d32f.frag`
pub const CONVERT_ABGR8_TO_D32F_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

layout(binding = 0) uniform sampler2D color_texture;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    vec4 color = texelFetch(color_texture, coord, 0).abgr;

    float value = color.a * (color.r + color.g + color.b) / 3.0f;

    gl_FragDepth = value;
}
"#;

/// Upstream: `host_shaders/convert_d24s8_to_abgr8.frag`
pub const CONVERT_D24S8_TO_ABGR8_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

precision mediump int;
precision highp float;

layout(binding = 0) uniform sampler2D depth_tex;
layout(binding = 1) uniform usampler2D stencil_tex;

layout(location = 0) out vec4 color;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    highp uint depth_val =
        uint(textureLod(depth_tex, coord, 0).r * (exp2(32.0) - 1.0));
    lowp uint stencil_val = textureLod(stencil_tex, coord, 0).r;
    highp uvec4 components =
        uvec4(stencil_val, (uvec3(depth_val) >> uvec3(24u, 16u, 8u)) & 0x000000FFu);
    color.abgr = vec4(components) / (exp2(8.0) - 1.0);
}
"#;

/// Upstream: `host_shaders/convert_d32f_to_abgr8.frag`
pub const CONVERT_D32F_TO_ABGR8_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

layout(binding = 0) uniform sampler2D depth_tex;

layout(location = 0) out vec4 color;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    float depth = texelFetch(depth_tex, coord, 0).r;
    color = vec4(depth, depth, depth, 1.0);
}
"#;

/// Upstream: `host_shaders/convert_depth_to_float.frag`
pub const CONVERT_DEPTH_TO_FLOAT_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

layout(binding = 0) uniform sampler2D depth_texture;
layout(location = 0) out float output_color;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    output_color = texelFetch(depth_texture, coord, 0).r;
}
"#;

/// Upstream: `host_shaders/convert_float_to_depth.frag`
pub const CONVERT_FLOAT_TO_DEPTH_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

layout(binding = 0) uniform sampler2D color_texture;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    float color = texelFetch(color_texture, coord, 0).r;
    gl_FragDepth = color;
}
"#;

/// Upstream: `host_shaders/convert_s8d24_to_abgr8.frag`
pub const CONVERT_S8D24_TO_ABGR8_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

precision mediump int;
precision highp float;

layout(binding = 0) uniform sampler2D depth_tex;
layout(binding = 1) uniform usampler2D stencil_tex;

layout(location = 0) out vec4 color;

void main() {
    ivec2 coord = ivec2(gl_FragCoord.xy);
    highp uint depth_val =
        uint(textureLod(depth_tex, coord, 0).r * (exp2(32.0) - 1.0));
    lowp uint stencil_val = textureLod(stencil_tex, coord, 0).r;
    highp uvec4 components =
        uvec4((uvec3(depth_val) >> uvec3(24u, 16u, 8u)) & 0x000000FFu, stencil_val);
    color.rgba = vec4(components) / (exp2(8.0) - 1.0);
}
"#;

/// Upstream: `host_shaders/fidelityfx_fsr.frag`
pub const FIDELITYFX_FSR_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//!#version 460 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_shader_explicit_arithmetic_types : require

// FidelityFX Super Resolution Sample
//
// Copyright (c) 2021 Advanced Micro Devices, Inc. All rights reserved.
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files(the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions :
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

layout( push_constant ) uniform constants {
    uvec4 Const0;
    uvec4 Const1;
    uvec4 Const2;
    uvec4 Const3;
};

layout(set=0,binding=0) uniform sampler2D InputTexture;

#define A_GPU 1
#define A_GLSL 1
#define FSR_RCAS_PASSTHROUGH_ALPHA 1

#ifndef YUZU_USE_FP16
    #include "ffx_a.h"

    #if USE_EASU
        #define FSR_EASU_F 1
        AF4 FsrEasuRF(AF2 p) { AF4 res = textureGather(InputTexture, p, 0); return res; }
        AF4 FsrEasuGF(AF2 p) { AF4 res = textureGather(InputTexture, p, 1); return res; }
        AF4 FsrEasuBF(AF2 p) { AF4 res = textureGather(InputTexture, p, 2); return res; }
    #endif
    #if USE_RCAS
        #define FSR_RCAS_F 1
        AF4 FsrRcasLoadF(ASU2 p) { return texelFetch(InputTexture, ASU2(p), 0); }
        void FsrRcasInputF(inout AF1 r, inout AF1 g, inout AF1 b) {}
    #endif
#else
    #define A_HALF
    #include "ffx_a.h"

    #if USE_EASU
        #define FSR_EASU_H 1
        AH4 FsrEasuRH(AF2 p) { AH4 res = AH4(textureGather(InputTexture, p, 0)); return res; }
        AH4 FsrEasuGH(AF2 p) { AH4 res = AH4(textureGather(InputTexture, p, 1)); return res; }
        AH4 FsrEasuBH(AF2 p) { AH4 res = AH4(textureGather(InputTexture, p, 2)); return res; }
    #endif
    #if USE_RCAS
        #define FSR_RCAS_H 1
        AH4 FsrRcasLoadH(ASW2 p) { return AH4(texelFetch(InputTexture, ASU2(p), 0)); }
        void FsrRcasInputH(inout AH1 r,inout AH1 g,inout AH1 b){}
    #endif
#endif

#include "ffx_fsr1.h"

layout (location = 0) in vec2 frag_texcoord;
layout (location = 0) out vec4 frag_color;

void CurrFilter(AU2 pos) {
#if USE_EASU
    #ifndef YUZU_USE_FP16
        AF3 c;
        FsrEasuF(c, pos, Const0, Const1, Const2, Const3);
        frag_color = AF4(c, texture(InputTexture, frag_texcoord).a);
    #else
        AH3 c;
        FsrEasuH(c, pos, Const0, Const1, Const2, Const3);
        frag_color = AH4(c, texture(InputTexture, frag_texcoord).a);
    #endif
#endif
#if USE_RCAS
    #ifndef YUZU_USE_FP16
        AF4 c;
        FsrRcasF(c.r, c.g, c.b, c.a, pos, Const0);
        frag_color = c;
    #else
        AH4 c;
        FsrRcasH(c.r, c.g, c.b, c.a, pos, Const0);
        frag_color = c;
    #endif
#endif
}

void main() {
#if USE_RCAS
    CurrFilter(AU2(frag_texcoord * vec2(textureSize(InputTexture, 0))));
#else
    CurrFilter(AU2(gl_FragCoord.xy));
#endif
}
"#;

/// Upstream: `host_shaders/fxaa.frag`
pub const FXAA_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

// Source code is adapted from
// https://www.geeks3d.com/20110405/fxaa-fast-approximate-anti-aliasing-demo-glsl-opengl-test-radeon-geforce/3/

#version 460

#ifdef VULKAN

#define BINDING_COLOR_TEXTURE 1

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#define BINDING_COLOR_TEXTURE 0

#endif

layout (location = 0) in vec4 posPos;

layout (location = 0) out vec4 frag_color;

layout (binding = BINDING_COLOR_TEXTURE) uniform sampler2D input_texture;

const float FXAA_SPAN_MAX = 8.0;
const float FXAA_REDUCE_MUL = 1.0 / 8.0;
const float FXAA_REDUCE_MIN = 1.0 / 128.0;

#define FxaaTexLod0(t, p) textureLod(t, p, 0.0)
#define FxaaTexOff(t, p, o) textureLodOffset(t, p, 0.0, o)

vec3 FxaaPixelShader(vec4 posPos, sampler2D tex) {

    vec3 rgbNW = FxaaTexLod0(tex, posPos.zw).xyz;
    vec3 rgbNE = FxaaTexOff(tex, posPos.zw, ivec2(1,0)).xyz;
    vec3 rgbSW = FxaaTexOff(tex, posPos.zw, ivec2(0,1)).xyz;
    vec3 rgbSE = FxaaTexOff(tex, posPos.zw, ivec2(1,1)).xyz;
    vec3 rgbM  = FxaaTexLod0(tex, posPos.xy).xyz;
/*---------------------------------------------------------*/
    vec3 luma = vec3(0.299, 0.587, 0.114);
    float lumaNW = dot(rgbNW, luma);
    float lumaNE = dot(rgbNE, luma);
    float lumaSW = dot(rgbSW, luma);
    float lumaSE = dot(rgbSE, luma);
    float lumaM  = dot(rgbM,  luma);
/*---------------------------------------------------------*/
    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));
/*---------------------------------------------------------*/
    vec2 dir;
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));
/*---------------------------------------------------------*/
    float dirReduce = max(
        (lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL),
        FXAA_REDUCE_MIN);
    float rcpDirMin = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);
    dir = min(vec2( FXAA_SPAN_MAX,  FXAA_SPAN_MAX),
          max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX),
          dir * rcpDirMin)) / textureSize(tex, 0);
/*--------------------------------------------------------*/
    vec3 rgbA = (1.0 / 2.0) * (
        FxaaTexLod0(tex, posPos.xy + dir * (1.0 / 3.0 - 0.5)).xyz +
        FxaaTexLod0(tex, posPos.xy + dir * (2.0 / 3.0 - 0.5)).xyz);
    vec3 rgbB = rgbA * (1.0 / 2.0) + (1.0 / 4.0) * (
        FxaaTexLod0(tex, posPos.xy + dir * (0.0 / 3.0 - 0.5)).xyz +
        FxaaTexLod0(tex, posPos.xy + dir * (3.0 / 3.0 - 0.5)).xyz);
    float lumaB = dot(rgbB, luma);
    if((lumaB < lumaMin) || (lumaB > lumaMax)) return rgbA;
    return rgbB;
}

void main() {
  frag_color = vec4(FxaaPixelShader(posPos, input_texture), texture(input_texture, posPos.xy).a);
}
"#;

/// Upstream: `host_shaders/opengl_fidelityfx_fsr.frag`
pub const OPENGL_FIDELITYFX_FSR_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//!#version 460 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

#extension GL_AMD_gpu_shader_half_float : enable
#extension GL_NV_gpu_shader5 : enable

// FidelityFX Super Resolution Sample
//
// Copyright (c) 2021 Advanced Micro Devices, Inc. All rights reserved.
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files(the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions :
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

layout (location = 0) uniform uvec4 constants[4];

#define A_GPU 1
#define A_GLSL 1
#define FSR_RCAS_PASSTHROUGH_ALPHA 1

#ifdef YUZU_USE_FP16
    #define A_HALF
#endif
#include "ffx_a.h"

#ifndef YUZU_USE_FP16
    layout (binding=0) uniform sampler2D InputTexture;
    #if USE_EASU
        #define FSR_EASU_F 1
        AF4 FsrEasuRF(AF2 p) { AF4 res = textureGather(InputTexture, p, 0); return res; }
        AF4 FsrEasuGF(AF2 p) { AF4 res = textureGather(InputTexture, p, 1); return res; }
        AF4 FsrEasuBF(AF2 p) { AF4 res = textureGather(InputTexture, p, 2); return res; }
    #endif
    #if USE_RCAS
        #define FSR_RCAS_F
        AF4 FsrRcasLoadF(ASU2 p) { return texelFetch(InputTexture, ASU2(p), 0); }
        void FsrRcasInputF(inout AF1 r, inout AF1 g, inout AF1 b) {}
    #endif
#else
    layout (binding=0) uniform sampler2D InputTexture;
    #if USE_EASU
        #define FSR_EASU_H 1
        AH4 FsrEasuRH(AF2 p) { AH4 res = AH4(textureGather(InputTexture, p, 0)); return res; }
        AH4 FsrEasuGH(AF2 p) { AH4 res = AH4(textureGather(InputTexture, p, 1)); return res; }
        AH4 FsrEasuBH(AF2 p) { AH4 res = AH4(textureGather(InputTexture, p, 2)); return res; }
    #endif
    #if USE_RCAS
        #define FSR_RCAS_H
        AH4 FsrRcasLoadH(ASW2 p) { return AH4(texelFetch(InputTexture, ASU2(p), 0)); }
        void FsrRcasInputH(inout AH1 r,inout AH1 g,inout AH1 b){}
    #endif
#endif

#include "ffx_fsr1.h"

layout (location = 0) in vec2 frag_texcoord;
layout (location = 0) out vec4 frag_color;

void CurrFilter(AU2 pos)
{
#if USE_EASU
    #ifndef YUZU_USE_FP16
        AF3 c;
        FsrEasuF(c, pos, constants[0], constants[1], constants[2], constants[3]);
        frag_color = AF4(c, texture(InputTexture, frag_texcoord).a);
    #else
        AH3 c;
        FsrEasuH(c, pos, constants[0], constants[1], constants[2], constants[3]);
        frag_color = AH4(c, texture(InputTexture, frag_texcoord).a);
    #endif
#endif
#if USE_RCAS
    #ifndef YUZU_USE_FP16
        AF4 c;
        FsrRcasF(c.r, c.g, c.b, c.a, pos, constants[0]);
        frag_color = c;
    #else
        AH3 c;
        FsrRcasH(c.r, c.g, c.b, c.a, pos, constants[0]);
        frag_color = c;
    #endif
#endif
}

void main()
{
#if USE_RCAS
    CurrFilter(AU2(frag_texcoord * vec2(textureSize(InputTexture, 0))));
#else
    CurrFilter(AU2(gl_FragCoord.xy));
#endif
}
"#;

/// Upstream: `host_shaders/opengl_fidelityfx_fsr_easu.frag`
pub const OPENGL_FIDELITYFX_FSR_EASU_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_GOOGLE_include_directive : enable

#define USE_EASU 1

#include "opengl_fidelityfx_fsr.frag"
"#;

/// Upstream: `host_shaders/opengl_fidelityfx_fsr_rcas.frag`
pub const OPENGL_FIDELITYFX_FSR_RCAS_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_GOOGLE_include_directive : enable

#define USE_RCAS 1

#include "opengl_fidelityfx_fsr.frag"
"#;

/// Upstream: `host_shaders/opengl_present.frag`
pub const OPENGL_PRESENT_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430 core

layout (location = 0) in vec2 frag_tex_coord;
layout (location = 0) out vec4 color;

layout (binding = 0) uniform sampler2D color_texture;

void main() {
    color = vec4(texture(color_texture, frag_tex_coord));
}
"#;

/// Upstream: `host_shaders/opengl_present_scaleforce.frag`
pub const OPENGL_PRESENT_SCALEFORCE_FRAG: &str = r#"// SPDX-FileCopyrightText: 2020 BreadFish64
// SPDX-License-Identifier: MIT

// Adapted from https://github.com/BreadFish64/ScaleFish/tree/master/scaleforce

//! #version 460

#extension GL_ARB_separate_shader_objects : enable

#ifdef YUZU_USE_FP16

#extension GL_AMD_gpu_shader_half_float : enable
#extension GL_NV_gpu_shader5 : enable

#define lfloat float16_t
#define lvec2 f16vec2
#define lvec3 f16vec3
#define lvec4 f16vec4

#else

#define lfloat float
#define lvec2 vec2
#define lvec3 vec3
#define lvec4 vec4

#endif

layout (location = 0) in vec2 tex_coord;

layout (location = 0) out vec4 frag_color;

layout (binding = 0) uniform sampler2D input_texture;

const bool ignore_alpha = true;

lfloat ColorDist1(lvec4 a, lvec4 b) {
    // https://en.wikipedia.org/wiki/YCbCr#ITU-R_BT.2020_conversion
    const lvec3 K = lvec3(0.2627, 0.6780, 0.0593);
    const lfloat scaleB = lfloat(0.5) / (lfloat(1.0) - K.b);
    const lfloat scaleR = lfloat(0.5) / (lfloat(1.0) - K.r);
    lvec4 diff = a - b;
    lfloat Y = dot(diff.rgb, K);
    lfloat Cb = scaleB * (diff.b - Y);
    lfloat Cr = scaleR * (diff.r - Y);
    lvec3 YCbCr = lvec3(Y, Cb, Cr);
    lfloat d = length(YCbCr);
    if (ignore_alpha) {
        return d;
    }
    return sqrt(a.a * b.a * d * d + diff.a * diff.a);
}

lvec4 ColorDist(lvec4 ref, lvec4 A, lvec4 B, lvec4 C, lvec4 D) {
    return lvec4(
            ColorDist1(ref, A),
            ColorDist1(ref, B),
            ColorDist1(ref, C),
            ColorDist1(ref, D)
        );
}

vec4 Scaleforce(sampler2D tex, vec2 tex_coord) {
    lvec4 bl = lvec4(textureOffset(tex, tex_coord, ivec2(-1, -1)));
    lvec4 bc = lvec4(textureOffset(tex, tex_coord, ivec2(0, -1)));
    lvec4 br = lvec4(textureOffset(tex, tex_coord, ivec2(1, -1)));
    lvec4 cl = lvec4(textureOffset(tex, tex_coord, ivec2(-1, 0)));
    lvec4 cc = lvec4(texture(tex, tex_coord));
    lvec4 cr = lvec4(textureOffset(tex, tex_coord, ivec2(1, 0)));
    lvec4 tl = lvec4(textureOffset(tex, tex_coord, ivec2(-1, 1)));
    lvec4 tc = lvec4(textureOffset(tex, tex_coord, ivec2(0, 1)));
    lvec4 tr = lvec4(textureOffset(tex, tex_coord, ivec2(1, 1)));

    lvec4 offset_tl = ColorDist(cc, tl, tc, tr, cr);
    lvec4 offset_br = ColorDist(cc, br, bc, bl, cl);

    // Calculate how different cc is from the texels around it
    const lfloat plus_weight = lfloat(1.5);
    const lfloat cross_weight = lfloat(1.5);
    lfloat total_dist = dot(offset_tl + offset_br, lvec4(cross_weight, plus_weight, cross_weight, plus_weight));

    if (total_dist == lfloat(0.0)) {
        return cc;
    } else {
        // Add together all the distances with direction taken into account
        lvec4 tmp = offset_tl - offset_br;
        lvec2 total_offset = tmp.wy * plus_weight + (tmp.zz + lvec2(-tmp.x, tmp.x)) * cross_weight;

        // When the image has thin points, they tend to split apart.
        // This is because the texels all around are different and total_offset reaches into clear areas.
        // This works pretty well to keep the offset in bounds for these cases.
        lfloat clamp_val = length(total_offset) / total_dist;
        vec2 final_offset = vec2(clamp(total_offset, -clamp_val, clamp_val)) / textureSize(tex, 0);

        return texture(tex, tex_coord - final_offset);
    }
}

void main() {
    frag_color = Scaleforce(input_texture, tex_coord);
}
"#;

/// Upstream: `host_shaders/present_bicubic.frag`
pub const PRESENT_BICUBIC_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core


layout (location = 0) in vec2 frag_tex_coord;

layout (location = 0) out vec4 color;

layout (binding = 0) uniform sampler2D color_texture;

vec4 cubic(float v) {
    vec4 n = vec4(1.0, 2.0, 3.0, 4.0) - v;
    vec4 s = n * n * n;
    float x = s.x;
    float y = s.y - 4.0 * s.x;
    float z = s.z - 4.0 * s.y + 6.0 * s.x;
    float w = 6.0 - x - y - z;
    return vec4(x, y, z, w) * (1.0 / 6.0);
}

vec4 textureBicubic( sampler2D textureSampler, vec2 texCoords ) {

    vec2 texSize = textureSize(textureSampler, 0);
    vec2 invTexSize = 1.0 / texSize;

    texCoords = texCoords * texSize - 0.5;

    vec2 fxy = fract(texCoords);
    texCoords -= fxy;

    vec4 xcubic = cubic(fxy.x);
    vec4 ycubic = cubic(fxy.y);

    vec4 c = texCoords.xxyy + vec2(-0.5, +1.5).xyxy;

    vec4 s = vec4(xcubic.xz + xcubic.yw, ycubic.xz + ycubic.yw);
    vec4 offset = c + vec4(xcubic.yw, ycubic.yw) / s;

    offset *= invTexSize.xxyy;

    vec4 sample0 = texture(textureSampler, offset.xz);
    vec4 sample1 = texture(textureSampler, offset.yz);
    vec4 sample2 = texture(textureSampler, offset.xw);
    vec4 sample3 = texture(textureSampler, offset.yw);

    float sx = s.x / (s.x + s.y);
    float sy = s.z / (s.z + s.w);

    return mix(mix(sample3, sample2, sx), mix(sample1, sample0, sx), sy);
}

void main() {
    color = textureBicubic(color_texture, frag_tex_coord);
}
"#;

/// Upstream: `host_shaders/present_gaussian.frag`
pub const PRESENT_GAUSSIAN_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

// Code adapted from the following sources:
// - https://learnopengl.com/Advanced-Lighting/Bloom
// - https://www.rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

#version 460 core

layout(location = 0) in vec2 frag_tex_coord;

layout(location = 0) out vec4 color;

layout(binding = 0) uniform sampler2D color_texture;

const float offset[3] = float[](0.0, 1.3846153846, 3.2307692308);
const float weight[3] = float[](0.2270270270, 0.3162162162, 0.0702702703);

vec4 blurVertical(sampler2D textureSampler, vec2 coord, vec2 norm) {
    vec4 result = vec4(0.0f);
    for (int i = 1; i < 3; i++) {
        result += texture(textureSampler, vec2(coord) + (vec2(0.0, offset[i]) * norm)) * weight[i];
        result += texture(textureSampler, vec2(coord) - (vec2(0.0, offset[i]) * norm)) * weight[i];
    }
    return result;
}

vec4 blurHorizontal(sampler2D textureSampler, vec2 coord, vec2 norm) {
    vec4 result = vec4(0.0f);
    for (int i = 1; i < 3; i++) {
        result += texture(textureSampler, vec2(coord) + (vec2(offset[i], 0.0) * norm)) * weight[i];
        result += texture(textureSampler, vec2(coord) - (vec2(offset[i], 0.0) * norm)) * weight[i];
    }
    return result;
}

vec4 blurDiagonal(sampler2D textureSampler, vec2 coord, vec2 norm) {
    vec4 result = vec4(0.0f);
    for (int i = 1; i < 3; i++) {
        result +=
            texture(textureSampler, vec2(coord) + (vec2(offset[i], offset[i]) * norm)) * weight[i];
        result +=
            texture(textureSampler, vec2(coord) - (vec2(offset[i], offset[i]) * norm)) * weight[i];
    }
    return result;
}

void main() {
    vec4 base = texture(color_texture, vec2(frag_tex_coord)) * weight[0];
    vec2 tex_offset = 1.0f / textureSize(color_texture, 0);

    // Upstream TODO(Blinkhawk): This code can be optimized through shader group instructions.
    vec4 horizontal = blurHorizontal(color_texture, frag_tex_coord, tex_offset);
    vec4 vertical = blurVertical(color_texture, frag_tex_coord, tex_offset);
    vec4 diagonalA = blurDiagonal(color_texture, frag_tex_coord, tex_offset);
    vec4 diagonalB = blurDiagonal(color_texture, frag_tex_coord, tex_offset * vec2(1.0, -1.0));
    vec4 combination = mix(mix(horizontal, vertical, 0.5f), mix(diagonalA, diagonalB, 0.5f), 0.5f);
    color = combination + base;
}
"#;

/// Upstream: `host_shaders/smaa_blending_weight_calculation.frag`
pub const SMAA_BLENDING_WEIGHT_CALCULATION_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

layout (binding = 0) uniform sampler2D edges_tex;
layout (binding = 1) uniform sampler2D area_tex;
layout (binding = 2) uniform sampler2D search_tex;

layout (location = 0) in vec2 tex_coord;
layout (location = 1) in vec2 pix_coord;
layout (location = 2) in vec4 offset[3];

layout (location = 0) out vec4 frag_color;

vec4 metrics = vec4(1.0 / textureSize(edges_tex, 0), textureSize(edges_tex, 0));
#define SMAA_RT_METRICS metrics
#define SMAA_GLSL_4
#define SMAA_PRESET_ULTRA
#define SMAA_INCLUDE_VS 0
#define SMAA_INCLUDE_PS 1

#include "opengl_smaa.glsl"

void main() {
    frag_color = SMAABlendingWeightCalculationPS(tex_coord,
                                       pix_coord,
                                       offset,
                                       edges_tex,
                                       area_tex,
                                       search_tex,
                                       vec4(0)
                                       );
}
"#;

/// Upstream: `host_shaders/smaa_edge_detection.frag`
pub const SMAA_EDGE_DETECTION_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

layout (binding = 0) uniform sampler2D input_tex;

layout (location = 0) in vec2 tex_coord;
layout (location = 1) in vec4 offset[3];

layout (location = 0) out vec2 frag_color;

vec4 metrics = vec4(1.0 / textureSize(input_tex, 0), textureSize(input_tex, 0));
#define SMAA_RT_METRICS metrics
#define SMAA_GLSL_4
#define SMAA_PRESET_ULTRA
#define SMAA_INCLUDE_VS 0
#define SMAA_INCLUDE_PS 1

#include "opengl_smaa.glsl"

void main() {
    frag_color = SMAAColorEdgeDetectionPS(tex_coord, offset, input_tex);
}
"#;

/// Upstream: `host_shaders/smaa_neighborhood_blending.frag`
pub const SMAA_NEIGHBORHOOD_BLENDING_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

layout (binding = 0) uniform sampler2D input_tex;
layout (binding = 1) uniform sampler2D blend_tex;

layout (location = 0) in vec2 tex_coord;
layout (location = 1) in vec4 offset;

layout (location = 0) out vec4 frag_color;

vec4 metrics = vec4(1.0 / textureSize(input_tex, 0), textureSize(input_tex, 0));
#define SMAA_RT_METRICS metrics
#define SMAA_GLSL_4
#define SMAA_PRESET_ULTRA
#define SMAA_INCLUDE_VS 0
#define SMAA_INCLUDE_PS 1

#include "opengl_smaa.glsl"

void main() {
    frag_color = SMAANeighborhoodBlendingPS(tex_coord,
                                  offset,
                                  input_tex,
                                  blend_tex
                                  );
}
"#;

/// Upstream: `host_shaders/vulkan_blit_depth_stencil.frag`
pub const VULKAN_BLIT_DEPTH_STENCIL_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450
#extension GL_ARB_shader_stencil_export : require

layout(binding = 0) uniform sampler2D depth_tex;
layout(binding = 1) uniform isampler2D stencil_tex;

layout(location = 0) in vec2 texcoord;

void main() {
    gl_FragDepth = textureLod(depth_tex, texcoord, 0).r;
    gl_FragStencilRefARB = textureLod(stencil_tex, texcoord, 0).r;
}
"#;

/// Upstream: `host_shaders/vulkan_color_clear.frag`
pub const VULKAN_COLOR_CLEAR_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

layout (push_constant) uniform PushConstants {
    vec4 clear_color;
};

layout(location = 0) out vec4 color;

void main() {
    color = clear_color;
}
"#;

/// Upstream: `host_shaders/vulkan_depthstencil_clear.frag`
pub const VULKAN_DEPTHSTENCIL_CLEAR_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

layout (push_constant) uniform PushConstants {
    vec4 clear_depth;
};

void main() {
    gl_FragDepth = clear_depth.x;
}
"#;

/// Upstream: `host_shaders/vulkan_fidelityfx_fsr_easu_fp16.frag`
pub const VULKAN_FIDELITYFX_FSR_EASU_FP16_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_GOOGLE_include_directive : enable

#define YUZU_USE_FP16
#define USE_EASU 1
#define VERSION 1

#include "fidelityfx_fsr.frag"
"#;

/// Upstream: `host_shaders/vulkan_fidelityfx_fsr_easu_fp32.frag`
pub const VULKAN_FIDELITYFX_FSR_EASU_FP32_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_GOOGLE_include_directive : enable

#define USE_EASU 1
#define VERSION 1

#include "fidelityfx_fsr.frag"
"#;

/// Upstream: `host_shaders/vulkan_fidelityfx_fsr_rcas_fp16.frag`
pub const VULKAN_FIDELITYFX_FSR_RCAS_FP16_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_GOOGLE_include_directive : enable

#define YUZU_USE_FP16
#define USE_RCAS 1
#define VERSION 1

#include "fidelityfx_fsr.frag"
"#;

/// Upstream: `host_shaders/vulkan_fidelityfx_fsr_rcas_fp32.frag`
pub const VULKAN_FIDELITYFX_FSR_RCAS_FP32_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_GOOGLE_include_directive : enable

#define USE_RCAS 1
#define VERSION 1

#include "fidelityfx_fsr.frag"
"#;

/// Upstream: `host_shaders/vulkan_present.frag`
pub const VULKAN_PRESENT_FRAG: &str = r#"// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

layout (location = 0) in vec2 frag_tex_coord;

layout (location = 0) out vec4 color;

layout (binding = 0) uniform sampler2D color_texture;

void main() {
    color = texture(color_texture, frag_tex_coord);
}
"#;

/// Upstream: `host_shaders/vulkan_present_scaleforce_fp16.frag`
pub const VULKAN_PRESENT_SCALEFORCE_FP16_FRAG: &str = r#"// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

#define VERSION 2
#define YUZU_USE_FP16

#include "opengl_present_scaleforce.frag"
"#;

/// Upstream: `host_shaders/vulkan_present_scaleforce_fp32.frag`
pub const VULKAN_PRESENT_SCALEFORCE_FP32_FRAG: &str = r#"// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460

#extension GL_GOOGLE_include_directive : enable

#define VERSION 2

#include "opengl_present_scaleforce.frag"
"#;

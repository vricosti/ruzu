// Embedded compute shaders (.comp) from upstream host_shaders/
// Maps to: /home/vricosti/shared/zuyu/src/video_core/host_shaders/

/// Upstream: `host_shaders/astc_decoder.comp`
pub const ASTC_DECODER_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450

#ifdef VULKAN

#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 1

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define UNIFORM(n) layout(location = n) uniform
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 0

#endif

layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

BEGIN_PUSH_CONSTANTS
UNIFORM(1) uvec2 block_dims;
UNIFORM(2) uint layer_stride;
UNIFORM(3) uint block_size;
UNIFORM(4) uint x_shift;
UNIFORM(5) uint block_height;
UNIFORM(6) uint block_height_mask;
END_PUSH_CONSTANTS

struct EncodingData {
    uint data;
};

layout(binding = BINDING_INPUT_BUFFER, std430) readonly restrict buffer InputBufferU32 {
    uvec4 astc_data[];
};

layout(binding = BINDING_OUTPUT_IMAGE, rgba8) uniform writeonly restrict image2DArray dest_image;

const uint GOB_SIZE_X_SHIFT = 6;
const uint GOB_SIZE_Y_SHIFT = 3;
const uint GOB_SIZE_SHIFT = GOB_SIZE_X_SHIFT + GOB_SIZE_Y_SHIFT;

const uint BYTES_PER_BLOCK_LOG2 = 4;

const uint JUST_BITS = 0u;
const uint QUINT = 1u;
const uint TRIT = 2u;

// ASTC Encodings data, sorted in ascending order based on their BitLength value
// (see GetBitLength() function)
const uint encoding_values[22] = uint[](
    (JUST_BITS), (JUST_BITS | (1u << 8u)), (TRIT), (JUST_BITS | (2u << 8u)),
    (QUINT), (TRIT | (1u << 8u)), (JUST_BITS | (3u << 8u)), (QUINT | (1u << 8u)),
    (TRIT | (2u << 8u)), (JUST_BITS | (4u << 8u)), (QUINT | (2u << 8u)), (TRIT | (3u << 8u)),
    (JUST_BITS | (5u << 8u)), (QUINT | (3u << 8u)), (TRIT | (4u << 8u)), (JUST_BITS | (6u << 8u)),
    (QUINT | (4u << 8u)), (TRIT | (5u << 8u)), (JUST_BITS | (7u << 8u)), (QUINT | (5u << 8u)),
    (TRIT | (6u << 8u)), (JUST_BITS | (8u << 8u)));

// Input ASTC texture globals
int total_bitsread = 0;
uvec4 local_buff;

// Color data globals
uvec4 color_endpoint_data;
int color_bitsread = 0;

// Global "vector" to be pushed into when decoding
// At most will require BLOCK_WIDTH x BLOCK_HEIGHT in single plane mode
// At most will require BLOCK_WIDTH x BLOCK_HEIGHT x 2 in dual plane mode
// So the maximum would be 144 (12 x 12) elements, x 2 for two planes
#define DIVCEIL(number, divisor) (number + divisor - 1) / divisor
#define ARRAY_NUM_ELEMENTS 144
#define VECTOR_ARRAY_SIZE DIVCEIL(ARRAY_NUM_ELEMENTS * 2, 4)
uint result_vector[ARRAY_NUM_ELEMENTS * 2];

int result_index = 0;
uint result_vector_max_index;
bool result_limit_reached = false;

// EncodingData helpers
uint Encoding(EncodingData val) {
    return bitfieldExtract(val.data, 0, 8);
}
uint NumBits(EncodingData val) {
    return bitfieldExtract(val.data, 8, 8);
}
uint BitValue(EncodingData val) {
    return bitfieldExtract(val.data, 16, 8);
}
uint QuintTritValue(EncodingData val) {
    return bitfieldExtract(val.data, 24, 8);
}

void Encoding(inout EncodingData val, uint v) {
    val.data = bitfieldInsert(val.data, v, 0, 8);
}
void NumBits(inout EncodingData val, uint v) {
    val.data = bitfieldInsert(val.data, v, 8, 8);
}
void BitValue(inout EncodingData val, uint v) {
    val.data = bitfieldInsert(val.data, v, 16, 8);
}
void QuintTritValue(inout EncodingData val, uint v) {
    val.data = bitfieldInsert(val.data, v, 24, 8);
}

EncodingData CreateEncodingData(uint encoding, uint num_bits, uint bit_val, uint quint_trit_val) {
    return EncodingData(((encoding) << 0u) | ((num_bits) << 8u) |
                        ((bit_val) << 16u) | ((quint_trit_val) << 24u));
}


void ResultEmplaceBack(EncodingData val) {
    if (result_index >= result_vector_max_index) {
        // Alert callers to avoid decoding more than needed by this phase
        result_limit_reached = true;
        return;
    }
    result_vector[result_index] = val.data;
    ++result_index;
}

uvec4 ReplicateByteTo16(uvec4 value) {
    return value * 0x101;
}

uint ReplicateBitTo7(uint value) {
    return value * 127;
}

uint ReplicateBitTo9(uint value) {
    return value * 511;
}

uint ReplicateBits(uint value, uint num_bits, uint to_bit) {
    if (value == 0 || num_bits == 0) {
        return 0;
    }
    if (num_bits >= to_bit) {
        return value;
    }
    const uint v = value & uint((1 << num_bits) - 1);
    uint res = v;
    uint reslen = num_bits;
    while (reslen < to_bit) {
        const uint num_dst_bits_to_shift_up = min(num_bits, to_bit - reslen);
        const uint num_src_bits_to_shift_down = num_bits - num_dst_bits_to_shift_up;

        res <<= num_dst_bits_to_shift_up;
        res |= (v >> num_src_bits_to_shift_down);
        reslen += num_bits;
    }
    return res;
}

uint FastReplicateTo8(uint value, uint num_bits) {
    return ReplicateBits(value, num_bits, 8);
}

uint FastReplicateTo6(uint value, uint num_bits) {
    return ReplicateBits(value, num_bits, 6);
}

uint Div3Floor(uint v) {
    return (v * 0x5556) >> 16;
}

uint Div3Ceil(uint v) {
    return Div3Floor(v + 2);
}

uint Div5Floor(uint v) {
    return (v * 0x3334) >> 16;
}

uint Div5Ceil(uint v) {
    return Div5Floor(v + 4);
}

uint Hash52(uint p) {
    p ^= p >> 15;
    p -= p << 17;
    p += p << 7;
    p += p << 4;
    p ^= p >> 5;
    p += p << 16;
    p ^= p >> 7;
    p ^= p >> 3;
    p ^= p << 6;
    p ^= p >> 17;
    return p;
}

uint Select2DPartition(uint seed, uint x, uint y, uint partition_count) {
    if ((block_dims.y * block_dims.x) < 32) {
        x <<= 1;
        y <<= 1;
    }

    seed += (partition_count - 1) * 1024;

    const uint rnum = Hash52(uint(seed));
    uint seed1 = uint(rnum & 0xF);
    uint seed2 = uint((rnum >> 4) & 0xF);
    uint seed3 = uint((rnum >> 8) & 0xF);
    uint seed4 = uint((rnum >> 12) & 0xF);
    uint seed5 = uint((rnum >> 16) & 0xF);
    uint seed6 = uint((rnum >> 20) & 0xF);
    uint seed7 = uint((rnum >> 24) & 0xF);
    uint seed8 = uint((rnum >> 28) & 0xF);

    seed1 = (seed1 * seed1);
    seed2 = (seed2 * seed2);
    seed3 = (seed3 * seed3);
    seed4 = (seed4 * seed4);
    seed5 = (seed5 * seed5);
    seed6 = (seed6 * seed6);
    seed7 = (seed7 * seed7);
    seed8 = (seed8 * seed8);

    uint sh1, sh2;
    if ((seed & 1) > 0) {
        sh1 = (seed & 2) > 0 ? 4 : 5;
        sh2 = (partition_count == 3) ? 6 : 5;
    } else {
        sh1 = (partition_count == 3) ? 6 : 5;
        sh2 = (seed & 2) > 0 ? 4 : 5;
    }
    seed1 >>= sh1;
    seed2 >>= sh2;
    seed3 >>= sh1;
    seed4 >>= sh2;
    seed5 >>= sh1;
    seed6 >>= sh2;
    seed7 >>= sh1;
    seed8 >>= sh2;

    uint a = seed1 * x + seed2 * y + (rnum >> 14);
    uint b = seed3 * x + seed4 * y + (rnum >> 10);
    uint c = seed5 * x + seed6 * y + (rnum >> 6);
    uint d = seed7 * x + seed8 * y + (rnum >> 2);

    a &= 0x3F;
    b &= 0x3F;
    c &= 0x3F;
    d &= 0x3F;

    if (partition_count < 4) {
        d = 0;
    }
    if (partition_count < 3) {
        c = 0;
    }

    if (a >= b && a >= c && a >= d) {
        return 0;
    } else if (b >= c && b >= d) {
        return 1;
    } else if (c >= d) {
        return 2;
    } else {
        return 3;
    }
}

uint ExtractBits(uvec4 payload, int offset, int bits) {
    if (bits <= 0) {
        return 0;
    }
    if (bits > 32) {
        return 0;
    }
    const int last_offset = offset + bits - 1;
    const int shifted_offset = offset >> 5;
    if ((last_offset >> 5) == shifted_offset) {
        return bitfieldExtract(payload[shifted_offset], offset & 31, bits);
    }
    const int first_bits = 32 - (offset & 31);
    const int result_first = int(bitfieldExtract(payload[shifted_offset], offset & 31, first_bits));
    const int result_second = int(bitfieldExtract(payload[shifted_offset + 1], 0, bits - first_bits));
    return result_first | (result_second << first_bits);
}

uint StreamBits(uint num_bits) {
    const int int_bits = int(num_bits);
    const uint ret = ExtractBits(local_buff, total_bitsread, int_bits);
    total_bitsread += int_bits;
    return ret;
}

void SkipBits(uint num_bits) {
    const int int_bits = int(num_bits);
    total_bitsread += int_bits;
}

uint StreamColorBits(uint num_bits) {
    const int int_bits = int(num_bits);
    const uint ret = ExtractBits(color_endpoint_data, color_bitsread, int_bits);
    color_bitsread += int_bits;
    return ret;
}

EncodingData GetEncodingFromVector(uint index) {
    const uint data = result_vector[index];
    return EncodingData(data);
}

// Returns the number of bits required to encode n_vals values.
uint GetBitLength(uint n_vals, uint encoding_index) {
    const EncodingData encoding_value = EncodingData(encoding_values[encoding_index]);
    const uint encoding = Encoding(encoding_value);
    uint total_bits = NumBits(encoding_value) * n_vals;
    if (encoding == TRIT) {
        total_bits += Div5Ceil(n_vals * 8);
    } else if (encoding == QUINT) {
        total_bits += Div3Ceil(n_vals * 7);
    }
    return total_bits;
}

uint GetNumWeightValues(uvec2 size, bool dual_plane) {
    uint n_vals = size.x * size.y;
    if (dual_plane) {
        n_vals *= 2;
    }
    return n_vals;
}

uint GetPackedBitSize(uvec2 size, bool dual_plane, uint max_weight) {
    const uint n_vals = GetNumWeightValues(size, dual_plane);
    return GetBitLength(n_vals, max_weight);
}

uint BitsBracket(uint bits, uint pos) {
    return ((bits >> pos) & 1);
}

uint BitsOp(uint bits, uint start, uint end) {
    const uint mask = (1 << (end - start + 1)) - 1;
    return ((bits >> start) & mask);
}

void DecodeQuintBlock(uint num_bits) {
    uvec3 m;
    uvec4 qQ;
    m[0] = StreamColorBits(num_bits);
    qQ.w = StreamColorBits(3);
    m[1] = StreamColorBits(num_bits);
    qQ.w |= StreamColorBits(2) << 3;
    m[2] = StreamColorBits(num_bits);
    qQ.w |= StreamColorBits(2) << 5;
    if (BitsOp(qQ.w, 1, 2) == 3 && BitsOp(qQ.w, 5, 6) == 0) {
        qQ.x = 4;
        qQ.y = 4;
        qQ.z = (BitsBracket(qQ.w, 0) << 2) | ((BitsBracket(qQ.w, 4) & ~BitsBracket(qQ.w, 0)) << 1) |
              (BitsBracket(qQ.w, 3) & ~BitsBracket(qQ.w, 0));
    } else {
        uint C = 0;
        if (BitsOp(qQ.w, 1, 2) == 3) {
            qQ.z = 4;
            C = (BitsOp(qQ.w, 3, 4) << 3) | ((~BitsOp(qQ.w, 5, 6) & 3) << 1) | BitsBracket(qQ.w, 0);
        } else {
            qQ.z = BitsOp(qQ.w, 5, 6);
            C = BitsOp(qQ.w, 0, 4);
        }
        if (BitsOp(C, 0, 2) == 5) {
            qQ.y = 4;
            qQ.x = BitsOp(C, 3, 4);
        } else {
            qQ.y = BitsOp(C, 3, 4);
            qQ.x = BitsOp(C, 0, 2);
        }
    }
    for (uint i = 0; i < 3; i++) {
        const EncodingData val = CreateEncodingData(QUINT, num_bits, m[i], qQ[i]);
        ResultEmplaceBack(val);
    }
}

void DecodeTritBlock(uint num_bits) {
    uvec4 m;
    uvec4 t;
    uvec3 Tm5t5;
    m[0] = StreamColorBits(num_bits);
    Tm5t5.x = StreamColorBits(2);
    m[1] = StreamColorBits(num_bits);
    Tm5t5.x |= StreamColorBits(2) << 2;
    m[2] = StreamColorBits(num_bits);
    Tm5t5.x |= StreamColorBits(1) << 4;
    m[3] = StreamColorBits(num_bits);
    Tm5t5.x |= StreamColorBits(2) << 5;
    Tm5t5.y = StreamColorBits(num_bits);
    Tm5t5.x |= StreamColorBits(1) << 7;
    uint C = 0;
    if (BitsOp(Tm5t5.x, 2, 4) == 7) {
        C = (BitsOp(Tm5t5.x, 5, 7) << 2) | BitsOp(Tm5t5.x, 0, 1);
        Tm5t5.z = 2;
        t[3] = 2;
    } else {
        C = BitsOp(Tm5t5.x, 0, 4);
        if (BitsOp(Tm5t5.x, 5, 6) == 3) {
            Tm5t5.z = 2;
            t[3] = BitsBracket(Tm5t5.x, 7);
        } else {
            Tm5t5.z = BitsBracket(Tm5t5.x, 7);
            t[3] = BitsOp(Tm5t5.x, 5, 6);
        }
    }
    if (BitsOp(C, 0, 1) == 3) {
        t[2] = 2;
        t[1] = BitsBracket(C, 4);
        t[0] = (BitsBracket(C, 3) << 1) | (BitsBracket(C, 2) & ~BitsBracket(C, 3));
    } else if (BitsOp(C, 2, 3) == 3) {
        t[2] = 2;
        t[1] = 2;
        t[0] = BitsOp(C, 0, 1);
    } else {
        t[2] = BitsBracket(C, 4);
        t[1] = BitsOp(C, 2, 3);
        t[0] = (BitsBracket(C, 1) << 1) | (BitsBracket(C, 0) & ~BitsBracket(C, 1));
    }
    for (uint i = 0; i < 4; i++) {
        const EncodingData val = CreateEncodingData(TRIT, num_bits, m[i], t[i]);
        ResultEmplaceBack(val);
    }
    const EncodingData val = CreateEncodingData(TRIT, num_bits, Tm5t5.y, Tm5t5.z);
    ResultEmplaceBack(val);
}

void DecodeIntegerSequence(uint max_range, uint num_values) {
    EncodingData val = EncodingData(encoding_values[max_range]);
    const uint encoding = Encoding(val);
    const uint num_bits = NumBits(val);
    uint vals_decoded = 0;
    while (vals_decoded < num_values && !result_limit_reached) {
        switch (encoding) {
        case QUINT:
            DecodeQuintBlock(num_bits);
            vals_decoded += 3;
            break;
        case TRIT:
            DecodeTritBlock(num_bits);
            vals_decoded += 5;
            break;
        case JUST_BITS:
            BitValue(val, StreamColorBits(num_bits));
            ResultEmplaceBack(val);
            vals_decoded++;
            break;
        }
    }
}

void DecodeColorValues(uvec4 modes, uint num_partitions, uint color_data_bits, out uint color_values[32]) {
    uint num_values = 0;
    for (uint i = 0; i < num_partitions; i++) {
        num_values += ((modes[i] >> 2) + 1) << 1;
    }
    // Find the largest encoding that's within color_data_bits
    // Upstream TODO(ameerj): profile with binary search
    int range = 0;
    while (++range < encoding_values.length()) {
        const uint bit_length = GetBitLength(num_values, range);
        if (bit_length > color_data_bits) {
            break;
        }
    }
    DecodeIntegerSequence(range - 1, num_values);
    uint out_index = 0;
    for (int itr = 0; itr < result_index; ++itr) {
        if (out_index >= num_values) {
            break;
        }
        const EncodingData val = GetEncodingFromVector(itr);
        const uint encoding = Encoding(val);
        const uint bitlen = NumBits(val);
        const uint bitval = BitValue(val);
        uint A = 0, B = 0, C = 0, D = 0;
        A = ReplicateBitTo9((bitval & 1));
        switch (encoding) {
        case JUST_BITS:
            color_values[++out_index] = FastReplicateTo8(bitval, bitlen);
            break;
        case TRIT: {
            D = QuintTritValue(val);
            switch (bitlen) {
            case 1:
                C = 204;
                break;
            case 2: {
                C = 93;
                const uint b = (bitval >> 1) & 1;
                B = (b << 8) | (b << 4) | (b << 2) | (b << 1);
                break;
            }
            case 3: {
                C = 44;
                const uint cb = (bitval >> 1) & 3;
                B = (cb << 7) | (cb << 2) | cb;
                break;
            }
            case 4: {
                C = 22;
                const uint dcb = (bitval >> 1) & 7;
                B = (dcb << 6) | dcb;
                break;
            }
            case 5: {
                C = 11;
                const uint edcb = (bitval >> 1) & 0xF;
                B = (edcb << 5) | (edcb >> 2);
                break;
            }
            case 6: {
                C = 5;
                const uint fedcb = (bitval >> 1) & 0x1F;
                B = (fedcb << 4) | (fedcb >> 4);
                break;
            }
            }
            break;
        }
        case QUINT: {
            D = QuintTritValue(val);
            switch (bitlen) {
            case 1:
                C = 113;
                break;
            case 2: {
                C = 54;
                const uint b = (bitval >> 1) & 1;
                B = (b << 8) | (b << 3) | (b << 2);
                break;
            }
            case 3: {
                C = 26;
                const uint cb = (bitval >> 1) & 3;
                B = (cb << 7) | (cb << 1) | (cb >> 1);
                break;
            }
            case 4: {
                C = 13;
                const uint dcb = (bitval >> 1) & 7;
                B = (dcb << 6) | (dcb >> 1);
                break;
            }
            case 5: {
                C = 6;
                const uint edcb = (bitval >> 1) & 0xF;
                B = (edcb << 5) | (edcb >> 3);
                break;
            }
            }
            break;
        }
        }
        if (encoding != JUST_BITS) {
            uint T = (D * C) + B;
            T ^= A;
            T = (A & 0x80) | (T >> 2);
            color_values[++out_index] = T;
        }
    }
}

ivec2 BitTransferSigned(int a, int b) {
    ivec2 transferred;
    transferred.y = b >> 1;
    transferred.y |= a & 0x80;
    transferred.x = a >> 1;
    transferred.x &= 0x3F;
    if ((transferred.x & 0x20) > 0) {
        transferred.x -= 0x40;
    }
    return transferred;
}

uvec4 ClampByte(ivec4 color) {
    return uvec4(clamp(color, 0, 255));
}

ivec4 BlueContract(int a, int r, int g, int b) {
    return ivec4(a, (r + b) >> 1, (g + b) >> 1, b);
}

void ComputeEndpoints(out uvec4 ep1, out uvec4 ep2, uint color_endpoint_mode, uint color_values[32],
                      inout uint colvals_index) {
#define READ_UINT_VALUES(N)                                                                        \
    uvec4 V[2];                                                                                    \
    for (uint i = 0; i < N; i++) {                                                                 \
        V[i / 4][i % 4] = color_values[++colvals_index];                      \
    }
#define READ_INT_VALUES(N)                                                                         \
    ivec4 V[2];                                                                                    \
    for (uint i = 0; i < N; i++) {                                                                 \
        V[i / 4][i % 4] = int(color_values[++colvals_index]);                      \
    }

    switch (color_endpoint_mode) {
    case 0: {
        READ_UINT_VALUES(2)
        ep1 = uvec4(0xFF, V[0].x, V[0].x, V[0].x);
        ep2 = uvec4(0xFF, V[0].y, V[0].y, V[0].y);
        break;
    }
    case 1: {
        READ_UINT_VALUES(2)
        const uint L0 = (V[0].x >> 2) | (V[0].y & 0xC0);
        const uint L1 = min(L0 + (V[0].y & 0x3F), 0xFFU);
        ep1 = uvec4(0xFF, L0, L0, L0);
        ep2 = uvec4(0xFF, L1, L1, L1);
        break;
    }
    case 4: {
        READ_UINT_VALUES(4)
        ep1 = uvec4(V[0].z, V[0].x, V[0].x, V[0].x);
        ep2 = uvec4(V[0].w, V[0].y, V[0].y, V[0].y);
        break;
    }
    case 5: {
        READ_INT_VALUES(4)
        ivec2 transferred = BitTransferSigned(V[0].y, V[0].x);
        V[0].y = transferred.x;
        V[0].x = transferred.y;
        transferred = BitTransferSigned(V[0].w, V[0].z);
        V[0].w = transferred.x;
        V[0].z = transferred.y;
        ep1 = ClampByte(ivec4(V[0].z, V[0].x, V[0].x, V[0].x));
        ep2 = ClampByte(ivec4(V[0].z + V[0].w, V[0].x + V[0].y, V[0].x + V[0].y, V[0].x + V[0].y));
        break;
    }
    case 6: {
        READ_UINT_VALUES(4)
        ep1 = uvec4(0xFF, (V[0].x * V[0].w) >> 8, (V[0].y * V[0].w) >> 8, (V[0].z * V[0].w) >> 8);
        ep2 = uvec4(0xFF, V[0].x, V[0].y, V[0].z);
        break;
    }
    case 8: {
        READ_UINT_VALUES(6)
        if ((V[0].y + V[0].w + V[1].y) >= (V[0].x + V[0].z + V[1].x)) {
            ep1 = uvec4(0xFF, V[0].x, V[0].z, V[1].x);
            ep2 = uvec4(0xFF, V[0].y, V[0].w, V[1].y);
        } else {
            ep1 = uvec4(BlueContract(0xFF, int(V[0].y), int(V[0].w), int(V[1].y)));
            ep2 = uvec4(BlueContract(0xFF, int(V[0].x), int(V[0].z), int(V[1].x)));
        }
        break;
    }
    case 9: {
        READ_INT_VALUES(6)
        ivec2 transferred = BitTransferSigned(V[0].y, V[0].x);
        V[0].y = transferred.x;
        V[0].x = transferred.y;
        transferred = BitTransferSigned(V[0].w, V[0].z);
        V[0].w = transferred.x;
        V[0].z = transferred.y;
        transferred = BitTransferSigned(V[1].y, V[1].x);
        V[1].y = transferred.x;
        V[1].x = transferred.y;
        if ((V[0].y + V[0].w + V[1].y) >= 0) {
            ep1 = ClampByte(ivec4(0xFF, V[0].x, V[0].z, V[1].x));
            ep2 = ClampByte(ivec4(0xFF, V[0].x + V[0].y, V[0].z + V[0].w, V[1].x + V[1].y));
        } else {
            ep1 = ClampByte(BlueContract(0xFF, V[0].x + V[0].y, V[0].z + V[0].w, V[1].x + V[1].y));
            ep2 = ClampByte(BlueContract(0xFF, V[0].x, V[0].z, V[1].x));
        }
        break;
    }
    case 10: {
        READ_UINT_VALUES(6)
        ep1 = uvec4(V[1].x, (V[0].x * V[0].w) >> 8, (V[0].y * V[0].w) >> 8, (V[0].z * V[0].w) >> 8);
        ep2 = uvec4(V[1].y, V[0].x, V[0].y, V[0].z);
        break;
    }
    case 12: {
        READ_UINT_VALUES(8)
        if ((V[0].y + V[0].w + V[1].y) >= (V[0].x + V[0].z + V[1].x)) {
            ep1 = uvec4(V[1].z, V[0].x, V[0].z, V[1].x);
            ep2 = uvec4(V[1].w, V[0].y, V[0].w, V[1].y);
        } else {
            ep1 = uvec4(BlueContract(int(V[1].w), int(V[0].y), int(V[0].w), int(V[1].y)));
            ep2 = uvec4(BlueContract(int(V[1].z), int(V[0].x), int(V[0].z), int(V[1].x)));
        }
        break;
    }
    case 13: {
        READ_INT_VALUES(8)
        ivec2 transferred = BitTransferSigned(V[0].y, V[0].x);
        V[0].y = transferred.x;
        V[0].x = transferred.y;
        transferred = BitTransferSigned(V[0].w, V[0].z);
        V[0].w = transferred.x;
        V[0].z = transferred.y;

        transferred = BitTransferSigned(V[1].y, V[1].x);
        V[1].y = transferred.x;
        V[1].x = transferred.y;

        transferred = BitTransferSigned(V[1].w, V[1].z);
        V[1].w = transferred.x;
        V[1].z = transferred.y;

        if ((V[0].y + V[0].w + V[1].y) >= 0) {
            ep1 = ClampByte(ivec4(V[1].z, V[0].x, V[0].z, V[1].x));
            ep2 = ClampByte(ivec4(V[1].w + V[1].z, V[0].x + V[0].y, V[0].z + V[0].w, V[1].x + V[1].y));
        } else {
            ep1 = ClampByte(BlueContract(V[1].z + V[1].w, V[0].x + V[0].y, V[0].z + V[0].w, V[1].x + V[1].y));
            ep2 = ClampByte(BlueContract(V[1].z, V[0].x, V[0].z, V[1].x));
        }
        break;
    }
    default: {
        // HDR mode, or more likely a bug computing the color_endpoint_mode
        ep1 = uvec4(0xFF, 0xFF, 0, 0);
        ep2 = uvec4(0xFF, 0xFF, 0, 0);
        break;
    }
    }
#undef READ_UINT_VALUES
#undef READ_INT_VALUES
}

uint UnquantizeTexelWeight(EncodingData val) {
    const uint encoding = Encoding(val);
    const uint bitlen = NumBits(val);
    const uint bitval = BitValue(val);
    const uint A = ReplicateBitTo7((bitval & 1));
    uint B = 0, C = 0, D = 0;
    uint result = 0;
    const uint bitlen_0_results[5] = {0, 16, 32, 48, 64};
    switch (encoding) {
    case JUST_BITS:
        return FastReplicateTo6(bitval, bitlen);
    case TRIT: {
        D = QuintTritValue(val);
        switch (bitlen) {
        case 0:
            return bitlen_0_results[D * 2];
        case 1: {
            C = 50;
            break;
        }
        case 2: {
            C = 23;
            const uint b = (bitval >> 1) & 1;
            B = (b << 6) | (b << 2) | b;
            break;
        }
        case 3: {
            C = 11;
            const uint cb = (bitval >> 1) & 3;
            B = (cb << 5) | cb;
            break;
        }
        default:
            break;
        }
        break;
    }
    case QUINT: {
        D = QuintTritValue(val);
        switch (bitlen) {
        case 0:
            return bitlen_0_results[D];
        case 1: {
            C = 28;
            break;
        }
        case 2: {
            C = 13;
            const uint b = (bitval >> 1) & 1;
            B = (b << 6) | (b << 1);
            break;
        }
        }
        break;
    }
    }
    if (encoding != JUST_BITS && bitlen > 0) {
        result = D * C + B;
        result ^= A;
        result = (A & 0x20) | (result >> 2);
    }
    if (result > 32) {
        result += 1;
    }
    return result;
}

void UnquantizeTexelWeights(uvec2 size, bool is_dual_plane) {
    const uint num_planes = is_dual_plane ? 2 : 1;
    const uint area = size.x * size.y;
    const uint loop_count = min(result_index, area * num_planes);
    for (uint itr = 0; itr < loop_count; ++itr) {
        result_vector[itr] =
            UnquantizeTexelWeight(GetEncodingFromVector(itr));
    }
}

uint GetUnquantizedTexelWeight(uint offset_base, uint plane, bool is_dual_plane) {
    const uint offset = is_dual_plane ? 2 * offset_base + plane : offset_base;
    return result_vector[offset];
}

uvec4 GetUnquantizedWeightVector(uint t, uint s, uvec2 size, uint plane_index, bool is_dual_plane) {
    const uint Ds = uint((block_dims.x * 0.5f + 1024) / (block_dims.x - 1));
    const uint Dt = uint((block_dims.y * 0.5f + 1024) / (block_dims.y - 1));
    const uint area = size.x * size.y;

    const uint cs = Ds * s;
    const uint ct = Dt * t;
    const uint gs = (cs * (size.x - 1) + 32) >> 6;
    const uint gt = (ct * (size.y - 1) + 32) >> 6;
    const uint js = gs >> 4;
    const uint fs = gs & 0xF;
    const uint jt = gt >> 4;
    const uint ft = gt & 0x0F;
    const uint w11 = (fs * ft + 8) >> 4;
    const uint w10 = ft - w11;
    const uint w01 = fs - w11;
    const uint w00 = 16 - fs - ft + w11;
    const uvec4 w = uvec4(w00, w01, w10, w11);
    const uint v0 = jt * size.x + js;

    uvec4 p0 = uvec4(0);
    uvec4 p1 = uvec4(0);

    if (v0 < area) {
        const uint offset_base = v0;
        p0.x = GetUnquantizedTexelWeight(offset_base, 0, is_dual_plane);
        p1.x = GetUnquantizedTexelWeight(offset_base, 1, is_dual_plane);
    }
    if ((v0 + 1) < (area)) {
        const uint offset_base = v0 + 1;
        p0.y = GetUnquantizedTexelWeight(offset_base, 0, is_dual_plane);
        p1.y = GetUnquantizedTexelWeight(offset_base, 1, is_dual_plane);
    }
    if ((v0 + size.x) < (area)) {
        const uint offset_base = v0 + size.x;
        p0.z = GetUnquantizedTexelWeight(offset_base, 0, is_dual_plane);
        p1.z = GetUnquantizedTexelWeight(offset_base, 1, is_dual_plane);
    }
    if ((v0 + size.x + 1) < (area)) {
        const uint offset_base = v0 + size.x + 1;
        p0.w = GetUnquantizedTexelWeight(offset_base, 0, is_dual_plane);
        p1.w = GetUnquantizedTexelWeight(offset_base, 1, is_dual_plane);
    }

    const uint primary_weight = (uint(dot(p0, w)) + 8) >> 4;

    uvec4 weight_vec = uvec4(primary_weight);

    if (is_dual_plane) {
        const uint secondary_weight = (uint(dot(p1, w)) + 8) >> 4;
        for (uint c = 0; c < 4; c++) {
            const bool is_secondary = ((plane_index + 1u) & 3u) == c;
            weight_vec[c] = is_secondary ? secondary_weight : primary_weight;
        }
    }
    return weight_vec;
}

int FindLayout(uint mode) {
    if ((mode & 3) != 0) {
        if ((mode & 8) != 0) {
            if ((mode & 4) != 0) {
                if ((mode & 0x100) != 0) {
                    return 4;
                }
                return 3;
            }
            return 2;
        }
        if ((mode & 4) != 0) {
            return 1;
        }
        return 0;
    }
    if ((mode & 0x100) != 0) {
        if ((mode & 0x80) != 0) {
            if ((mode & 0x20) != 0) {
                return 8;
            }
            return 7;
        }
        return 9;
    }
    if ((mode & 0x80) != 0) {
        return 6;
    }
    return 5;
}


void FillError(ivec3 coord) {
    for (uint j = 0; j < block_dims.y; j++) {
        for (uint i = 0; i < block_dims.x; i++) {
            imageStore(dest_image, coord + ivec3(i, j, 0), vec4(0.0, 0.0, 0.0, 0.0));
        }
    }
}

void FillVoidExtentLDR(ivec3 coord) {
    SkipBits(52);
    const uint r_u = StreamBits(16);
    const uint g_u = StreamBits(16);
    const uint b_u = StreamBits(16);
    const uint a_u = StreamBits(16);
    const float a = float(a_u) / 65535.0f;
    const float r = float(r_u) / 65535.0f;
    const float g = float(g_u) / 65535.0f;
    const float b = float(b_u) / 65535.0f;
    for (uint j = 0; j < block_dims.y; j++) {
        for (uint i = 0; i < block_dims.x; i++) {
            imageStore(dest_image, coord + ivec3(i, j, 0), vec4(r, g, b, a));
        }
    }
}

bool IsError(uint mode) {
    if ((mode & 0x1ff) == 0x1fc) {
        if ((mode & 0x200) != 0) {
            // params.void_extent_hdr = true;
            return true;
        }
        if ((mode & 0x400) == 0 || StreamBits(1) == 0) {
            return true;
        }
        return false;
    }
    if ((mode & 0xf) == 0) {
        return true;
    }
    if ((mode & 3) == 0 && (mode & 0x1c0) == 0x1c0) {
        return true;
    }
    return false;
}

uvec2 DecodeBlockSize(uint mode) {
    uint A, B;
    switch (FindLayout(mode)) {
    case 0:
        A = (mode >> 5) & 0x3;
        B = (mode >> 7) & 0x3;
        return uvec2(B + 4, A + 2);
    case 1:
        A = (mode >> 5) & 0x3;
        B = (mode >> 7) & 0x3;
        return uvec2(B + 8, A + 2);
    case 2:
        A = (mode >> 5) & 0x3;
        B = (mode >> 7) & 0x3;
        return uvec2(A + 2, B + 8);
    case 3:
        A = (mode >> 5) & 0x3;
        B = (mode >> 7) & 0x1;
        return uvec2(A + 2, B + 6);
    case 4:
        A = (mode >> 5) & 0x3;
        B = (mode >> 7) & 0x1;
        return uvec2(B + 2, A + 2);
    case 5:
        A = (mode >> 5) & 0x3;
        return uvec2(12, A + 2);
    case 6:
        A = (mode >> 5) & 0x3;
        return uvec2(A + 2, 12);
    case 7:
        return uvec2(6, 10);
    case 8:
        return uvec2(10, 6);
    case 9:
        A = (mode >> 5) & 0x3;
        B = (mode >> 9) & 0x3;
        return uvec2(A + 6, B + 6);
    default:
        return uvec2(0);
    }
}

uint DecodeMaxWeight(uint mode) {
    const uint mode_layout = FindLayout(mode);
    uint weight_index = (mode & 0x10) != 0 ? 1 : 0;
    if (mode_layout < 5) {
        weight_index |= (mode & 0x3) << 1;
    } else {
        weight_index |= (mode & 0xc) >> 1;
    }
    weight_index -= 2;
    if ((mode_layout != 9) && ((mode & 0x200) != 0)) {
        weight_index += 6;
    }
    return weight_index + 1;
}

void DecompressBlock(ivec3 coord) {
    uint mode = StreamBits(11);
    if (IsError(mode)) {
        FillError(coord);
        return;
    }
    if ((mode & 0x1ff) == 0x1fc) {
        // params.void_extent_ldr = true;
        FillVoidExtentLDR(coord);
        return;
    }
    const uvec2 size_params = DecodeBlockSize(mode);
    if ((size_params.x > block_dims.x) || (size_params.y > block_dims.y)) {
        FillError(coord);
        return;
    }
    const uint num_partitions = StreamBits(2) + 1;
    const uint mode_layout = FindLayout(mode);
    const bool dual_plane = (mode_layout != 9) && ((mode & 0x400) != 0);
    if (num_partitions > 4 || (num_partitions == 4 && dual_plane)) {
        FillError(coord);
        return;
    }
    uint partition_index = 1;
    uvec4 color_endpoint_mode = uvec4(0);
    uint ced_pointer = 0;
    uint base_cem = 0;
    if (num_partitions == 1) {
        color_endpoint_mode.x = StreamBits(4);
        partition_index = 0;
    } else {
        partition_index = StreamBits(10);
        base_cem = StreamBits(6);
    }
    const uint base_mode = base_cem & 3;
    const uint max_weight = DecodeMaxWeight(mode);
    const uint weight_bits = GetPackedBitSize(size_params, dual_plane, max_weight);
    uint remaining_bits = 128 - weight_bits - total_bitsread;
    uint extra_cem_bits = 0;
    if (base_mode > 0) {
        switch (num_partitions) {
        case 2:
            extra_cem_bits += 2;
            break;
        case 3:
            extra_cem_bits += 5;
            break;
        case 4:
            extra_cem_bits += 8;
            break;
        default:
            return;
        }
    }
    remaining_bits -= extra_cem_bits;
    const uint plane_selector_bits = dual_plane ? 2 : 0;
    remaining_bits -= plane_selector_bits;
    if (remaining_bits > 128) {
        // Bad data, more remaining bits than 4 bytes
        // return early
        return;
    }
    // Read color data...
    const uint color_data_bits = remaining_bits;
    while (remaining_bits > 0) {
        const int nb = int(min(remaining_bits, 32U));
        const uint b = StreamBits(nb);
        color_endpoint_data[ced_pointer] = uint(bitfieldExtract(b, 0, nb));
        ++ced_pointer;
        remaining_bits -= nb;
    }
    const uint plane_index = uint(StreamBits(plane_selector_bits));
    if (base_mode > 0) {
        const uint extra_cem = StreamBits(extra_cem_bits);
        uint cem = (extra_cem << 6) | base_cem;
        cem >>= 2;
        uvec4 C = uvec4(0);
        for (uint i = 0; i < num_partitions; i++) {
            C[i] = (cem & 1);
            cem >>= 1;
        }
        uvec4 M = uvec4(0);
        for (uint i = 0; i < num_partitions; i++) {
            M[i] = cem & 3;
            cem >>= 2;
        }
        for (uint i = 0; i < num_partitions; i++) {
            color_endpoint_mode[i] = base_mode;
            if (C[i] == 0) {
                --color_endpoint_mode[i];
            }
            color_endpoint_mode[i] <<= 2;
            color_endpoint_mode[i] |= M[i];
        }
    } else if (num_partitions > 1) {
        const uint cem = base_cem >> 2;
        for (uint i = 0; i < num_partitions; i++) {
            color_endpoint_mode[i] = cem;
        }
    }

    uvec4 endpoints0[4];
    uvec4 endpoints1[4];
    {
        // This decode phase should at most push 32 elements into the vector
        result_vector_max_index = 32;
        uint color_values[32];
        uint colvals_index = 0;
        DecodeColorValues(color_endpoint_mode, num_partitions, color_data_bits, color_values);
        for (uint i = 0; i < num_partitions; i++) {
            ComputeEndpoints(endpoints0[i], endpoints1[i], color_endpoint_mode[i], color_values,
                             colvals_index);
        }
    }
    color_endpoint_data = local_buff;
    color_endpoint_data = bitfieldReverse(color_endpoint_data).wzyx;
    const uint clear_byte_start = (weight_bits >> 3) + 1;

    const uint byte_insert = ExtractBits(color_endpoint_data, int(clear_byte_start - 1) * 8, 8) &
                             uint(((1 << (weight_bits % 8)) - 1));
    const uint vec_index = (clear_byte_start - 1) >> 2;
    color_endpoint_data[vec_index] = bitfieldInsert(color_endpoint_data[vec_index], byte_insert,
                                                    int((clear_byte_start - 1) % 4) * 8, 8);
    for (uint i = clear_byte_start; i < 16; ++i) {
        const uint idx = i >> 2;
        color_endpoint_data[idx] = bitfieldInsert(color_endpoint_data[idx], 0, int(i % 4) * 8, 8);
    }

    // Re-init vector variables for next decode phase
    result_index = 0;
    color_bitsread = 0;
    result_limit_reached = false;

    // The limit for the Unquantize phase, avoids decoding more data than needed.
    result_vector_max_index = size_params.x * size_params.y;
    if (dual_plane) {
        result_vector_max_index *= 2;
    }
    DecodeIntegerSequence(max_weight, GetNumWeightValues(size_params, dual_plane));

    UnquantizeTexelWeights(size_params, dual_plane);
    for (uint j = 0; j < block_dims.y; j++) {
        for (uint i = 0; i < block_dims.x; i++) {
            uint local_partition = 0;
            if (num_partitions > 1) {
                local_partition = Select2DPartition(partition_index, i, j, num_partitions);
            }
            const uvec4 C0 = ReplicateByteTo16(endpoints0[local_partition]);
            const uvec4 C1 = ReplicateByteTo16(endpoints1[local_partition]);
            const uvec4 weight_vec = GetUnquantizedWeightVector(j, i, size_params, plane_index, dual_plane);
            const vec4 Cf =
                vec4((C0 * (uvec4(64) - weight_vec) + C1 * weight_vec + uvec4(32)) / 64);
            const vec4 p = (Cf / 65535.0f);
            imageStore(dest_image, coord + ivec3(i, j, 0), p.gbar);
        }
    }
}

uint SwizzleOffset(uvec2 pos) {
    const uint x = pos.x;
    const uint y = pos.y;
    return ((x % 64) / 32) * 256 + ((y % 8) / 2) * 64 +
            ((x % 32) / 16) * 32 + (y % 2) * 16 + (x % 16);
}

void main() {
    uvec3 pos = gl_GlobalInvocationID;
    pos.x <<= BYTES_PER_BLOCK_LOG2;
    const uint swizzle = SwizzleOffset(pos.xy);
    const uint block_y = pos.y >> GOB_SIZE_Y_SHIFT;

    uint offset = 0;
    offset += pos.z * layer_stride;
    offset += (block_y >> block_height) * block_size;
    offset += (block_y & block_height_mask) << GOB_SIZE_SHIFT;
    offset += (pos.x >> GOB_SIZE_X_SHIFT) << x_shift;
    offset += swizzle;

    const ivec3 coord = ivec3(gl_GlobalInvocationID * uvec3(block_dims, 1));
    if (any(greaterThanEqual(coord, imageSize(dest_image)))) {
        return;
    }
    local_buff = astc_data[offset / 16];
    DecompressBlock(coord);
}
"#;

/// Upstream: `host_shaders/block_linear_unswizzle_2d.comp`
pub const BLOCK_LINEAR_UNSWIZZLE_2D_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430

#ifdef VULKAN

#extension GL_EXT_shader_16bit_storage : require
#extension GL_EXT_shader_8bit_storage : require
#define HAS_EXTENDED_TYPES 1
#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define BINDING_SWIZZLE_BUFFER 0
#define BINDING_INPUT_BUFFER 1
#define BINDING_OUTPUT_IMAGE 2

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#extension GL_NV_gpu_shader5 : enable
#ifdef GL_NV_gpu_shader5
#define HAS_EXTENDED_TYPES 1
#else
#define HAS_EXTENDED_TYPES 0
#endif
#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define UNIFORM(n) layout (location = n) uniform
#define BINDING_SWIZZLE_BUFFER 0
#define BINDING_INPUT_BUFFER 1
#define BINDING_OUTPUT_IMAGE 0

#endif

BEGIN_PUSH_CONSTANTS
UNIFORM(0) uvec3 origin;
UNIFORM(1) ivec3 destination;
UNIFORM(2) uint bytes_per_block_log2;
UNIFORM(3) uint layer_stride;
UNIFORM(4) uint block_size;
UNIFORM(5) uint x_shift;
UNIFORM(6) uint block_height;
UNIFORM(7) uint block_height_mask;
END_PUSH_CONSTANTS

layout(binding = BINDING_SWIZZLE_BUFFER, std430) readonly buffer SwizzleTable {
    uint swizzle_table[];
};

#if HAS_EXTENDED_TYPES
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU8 { uint8_t u8data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU16 { uint16_t u16data[]; };
#endif
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU32 { uint u32data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU64 { uvec2 u64data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU128 { uvec4 u128data[]; };

layout(binding = BINDING_OUTPUT_IMAGE) uniform writeonly uimage2DArray output_image;

layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;

const uint GOB_SIZE_X = 64;
const uint GOB_SIZE_Y = 8;
const uint GOB_SIZE_Z = 1;
const uint GOB_SIZE = GOB_SIZE_X * GOB_SIZE_Y * GOB_SIZE_Z;

const uint GOB_SIZE_X_SHIFT = 6;
const uint GOB_SIZE_Y_SHIFT = 3;
const uint GOB_SIZE_Z_SHIFT = 0;
const uint GOB_SIZE_SHIFT = GOB_SIZE_X_SHIFT + GOB_SIZE_Y_SHIFT + GOB_SIZE_Z_SHIFT;

const uvec2 SWIZZLE_MASK = uvec2(GOB_SIZE_X - 1, GOB_SIZE_Y - 1);

uint SwizzleOffset(uvec2 pos) {
    pos = pos & SWIZZLE_MASK;
    return swizzle_table[pos.y * 64 + pos.x];
}

uvec4 ReadTexel(uint offset) {
    switch (bytes_per_block_log2) {
#if HAS_EXTENDED_TYPES
    case 0:
        return uvec4(u8data[offset], 0, 0, 0);
    case 1:
        return uvec4(u16data[offset / 2], 0, 0, 0);
#else
    case 0:
        return uvec4(bitfieldExtract(u32data[offset / 4], int((offset * 8) & 24), 8), 0, 0, 0);
    case 1:
        return uvec4(bitfieldExtract(u32data[offset / 4], int((offset * 8) & 16), 16), 0, 0, 0);
#endif
    case 2:
        return uvec4(u32data[offset / 4], 0, 0, 0);
    case 3:
        return uvec4(u64data[offset / 8], 0, 0);
    case 4:
        return u128data[offset / 16];
    }
    return uvec4(0);
}

void main() {
    uvec3 pos = gl_GlobalInvocationID + origin;
    pos.x <<= bytes_per_block_log2;

    // Read as soon as possible due to its latency
    const uint swizzle = SwizzleOffset(pos.xy);

    const uint block_y = pos.y >> GOB_SIZE_Y_SHIFT;

    uint offset = 0;
    offset += pos.z * layer_stride;
    offset += (block_y >> block_height) * block_size;
    offset += (block_y & block_height_mask) << GOB_SIZE_SHIFT;
    offset += (pos.x >> GOB_SIZE_X_SHIFT) << x_shift;
    offset += swizzle;

    const uvec4 texel = ReadTexel(offset);
    const ivec3 coord = ivec3(gl_GlobalInvocationID) + destination;
    imageStore(output_image, coord, texel);
}
"#;

/// Upstream: `host_shaders/block_linear_unswizzle_3d.comp`
pub const BLOCK_LINEAR_UNSWIZZLE_3D_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430

#ifdef VULKAN

#extension GL_EXT_shader_16bit_storage : require
#extension GL_EXT_shader_8bit_storage : require
#define HAS_EXTENDED_TYPES 1
#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define BINDING_SWIZZLE_BUFFER 0
#define BINDING_INPUT_BUFFER 1
#define BINDING_OUTPUT_IMAGE 2

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#extension GL_NV_gpu_shader5 : enable
#ifdef GL_NV_gpu_shader5
#define HAS_EXTENDED_TYPES 1
#else
#define HAS_EXTENDED_TYPES 0
#endif
#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define UNIFORM(n) layout (location = n) uniform
#define BINDING_SWIZZLE_BUFFER 0
#define BINDING_INPUT_BUFFER 1
#define BINDING_OUTPUT_IMAGE 0

#endif

BEGIN_PUSH_CONSTANTS
UNIFORM(0) uvec3 origin;
UNIFORM(1) ivec3 destination;
UNIFORM(2) uint bytes_per_block_log2;
UNIFORM(3) uint slice_size;
UNIFORM(4) uint block_size;
UNIFORM(5) uint x_shift;
UNIFORM(6) uint block_height;
UNIFORM(7) uint block_height_mask;
UNIFORM(8) uint block_depth;
UNIFORM(9) uint block_depth_mask;
END_PUSH_CONSTANTS

layout(binding = BINDING_SWIZZLE_BUFFER, std430) readonly buffer SwizzleTable {
    uint swizzle_table[];
};

#if HAS_EXTENDED_TYPES
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU8 { uint8_t u8data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU16 { uint16_t u16data[]; };
#endif
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU32 { uint u32data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU64 { uvec2 u64data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) buffer InputBufferU128 { uvec4 u128data[]; };

layout(binding = BINDING_OUTPUT_IMAGE) uniform writeonly uimage3D output_image;

layout(local_size_x = 16, local_size_y = 8, local_size_z = 8) in;

const uint GOB_SIZE_X = 64;
const uint GOB_SIZE_Y = 8;
const uint GOB_SIZE_Z = 1;
const uint GOB_SIZE = GOB_SIZE_X * GOB_SIZE_Y * GOB_SIZE_Z;

const uint GOB_SIZE_X_SHIFT = 6;
const uint GOB_SIZE_Y_SHIFT = 3;
const uint GOB_SIZE_Z_SHIFT = 0;
const uint GOB_SIZE_SHIFT = GOB_SIZE_X_SHIFT + GOB_SIZE_Y_SHIFT + GOB_SIZE_Z_SHIFT;

const uvec2 SWIZZLE_MASK = uvec2(GOB_SIZE_X - 1, GOB_SIZE_Y - 1);

uint SwizzleOffset(uvec2 pos) {
    pos = pos & SWIZZLE_MASK;
    return swizzle_table[pos.y * 64 + pos.x];
}

uvec4 ReadTexel(uint offset) {
    switch (bytes_per_block_log2) {
#if HAS_EXTENDED_TYPES
    case 0:
        return uvec4(u8data[offset], 0, 0, 0);
    case 1:
        return uvec4(u16data[offset / 2], 0, 0, 0);
#else
    case 0:
        return uvec4(bitfieldExtract(u32data[offset / 4], int((offset * 8) & 24), 8), 0, 0, 0);
    case 1:
        return uvec4(bitfieldExtract(u32data[offset / 4], int((offset * 8) & 16), 16), 0, 0, 0);
#endif
    case 2:
        return uvec4(u32data[offset / 4], 0, 0, 0);
    case 3:
        return uvec4(u64data[offset / 8], 0, 0);
    case 4:
        return u128data[offset / 16];
    }
    return uvec4(0);
}

void main() {
    uvec3 pos = gl_GlobalInvocationID + origin;
    pos.x <<= bytes_per_block_log2;

    // Read as soon as possible due to its latency
    const uint swizzle = SwizzleOffset(pos.xy);

    const uint block_y = pos.y >> GOB_SIZE_Y_SHIFT;

    uint offset = 0;
    offset += (pos.z >> block_depth) * slice_size;
    offset += (pos.z & block_depth_mask) << (GOB_SIZE_SHIFT + block_height);
    offset += (block_y >> block_height) * block_size;
    offset += (block_y & block_height_mask) << GOB_SIZE_SHIFT;
    offset += (pos.x >> GOB_SIZE_X_SHIFT) << x_shift;
    offset += swizzle;

    const uvec4 texel = ReadTexel(offset);
    const ivec3 coord = ivec3(gl_GlobalInvocationID) + destination;
    imageStore(output_image, coord, texel);
}
"#;

/// Upstream: `host_shaders/convert_msaa_to_non_msaa.comp`
pub const CONVERT_MSAA_TO_NON_MSAA_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450 core
layout (local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

layout (binding = 0, rgba8) uniform readonly restrict image2DMSArray msaa_in;
layout (binding = 1, rgba8) uniform writeonly restrict image2DArray output_img;

void main() {
    const ivec3 coords = ivec3(gl_GlobalInvocationID);
    if (any(greaterThanEqual(coords, imageSize(msaa_in)))) {
        return;
    }

    // Upstream TODO: Specialization constants for num_samples?
    const int num_samples = imageSamples(msaa_in);
    const ivec3 msaa_size = imageSize(msaa_in);
    const ivec3 out_size = imageSize(output_img);
    const ivec3 scale = out_size / msaa_size;
    for (int curr_sample = 0; curr_sample < num_samples; ++curr_sample) {
        const vec4 pixel = imageLoad(msaa_in, coords, curr_sample);

        const int single_sample_x = scale.x * coords.x + (curr_sample & 1);
        const int single_sample_y = scale.y * coords.y + ((curr_sample / 2) & 1);
        const ivec3 dest_coords = ivec3(single_sample_x, single_sample_y, coords.z);

        if (any(greaterThanEqual(dest_coords, imageSize(output_img)))) {
            continue;
        }
        imageStore(output_img, dest_coords, pixel);
    }
}
"#;

/// Upstream: `host_shaders/convert_non_msaa_to_msaa.comp`
pub const CONVERT_NON_MSAA_TO_MSAA_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 450 core
layout (local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

layout (binding = 0, rgba8) uniform readonly restrict image2DArray img_in;
layout (binding = 1, rgba8) uniform writeonly restrict image2DMSArray output_msaa;

void main() {
    const ivec3 coords = ivec3(gl_GlobalInvocationID);
    if (any(greaterThanEqual(coords, imageSize(output_msaa)))) {
        return;
    }

    // Upstream TODO: Specialization constants for num_samples?
    const int num_samples = imageSamples(output_msaa);
    const ivec3 msaa_size = imageSize(output_msaa);
    const ivec3 out_size = imageSize(img_in);
    const ivec3 scale = out_size / msaa_size;
    for (int curr_sample = 0; curr_sample < num_samples; ++curr_sample) {
        const int single_sample_x = scale.x * coords.x + (curr_sample & 1);
        const int single_sample_y = scale.y * coords.y + ((curr_sample / 2) & 1);
        const ivec3 single_coords = ivec3(single_sample_x, single_sample_y, coords.z);

        if (any(greaterThanEqual(single_coords, imageSize(img_in)))) {
            continue;
        }
        const vec4 pixel = imageLoad(img_in, single_coords);
        imageStore(output_msaa, coords, curr_sample, pixel);
    }
}
"#;

/// Upstream: `host_shaders/opengl_convert_s8d24.comp`
pub const OPENGL_CONVERT_S8D24_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430 core

layout(local_size_x = 16, local_size_y = 8) in;

layout(binding = 0, rgba8ui) restrict uniform uimage2D destination;
layout(location = 0) uniform uvec3 size;

void main() {
    if (any(greaterThanEqual(gl_GlobalInvocationID, size))) {
        return;
    }
    uvec4 components = imageLoad(destination, ivec2(gl_GlobalInvocationID.xy));
    imageStore(destination, ivec2(gl_GlobalInvocationID.xy), components.wxyz);
}
"#;

/// Upstream: `host_shaders/opengl_copy_bc4.comp`
pub const OPENGL_COPY_BC4_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430 core
#extension GL_ARB_gpu_shader_int64 : require

layout (local_size_x = 4, local_size_y = 4) in;

layout(binding = 0, rg32ui) readonly uniform uimage3D bc4_input;
layout(binding = 1, rgba8ui) writeonly uniform uimage3D bc4_output;

layout(location = 0) uniform uvec3 src_offset;
layout(location = 1) uniform uvec3 dst_offset;

// https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_compression_rgtc.txt
uint DecompressBlock(uint64_t bits, uvec2 coord) {
    const uint code_offset = 16 + 3 * (4 * coord.y + coord.x);
    const uint code = uint(bits >> code_offset) & 7;
    const uint red0 = uint(bits >> 0) & 0xff;
    const uint red1 = uint(bits >> 8) & 0xff;
    if (red0 > red1) {
        switch (code) {
        case 0:
            return red0;
        case 1:
            return red1;
        case 2:
            return (6 * red0 + 1 * red1) / 7;
        case 3:
            return (5 * red0 + 2 * red1) / 7;
        case 4:
            return (4 * red0 + 3 * red1) / 7;
        case 5:
            return (3 * red0 + 4 * red1) / 7;
        case 6:
            return (2 * red0 + 5 * red1) / 7;
        case 7:
            return (1 * red0 + 6 * red1) / 7;
        }
    } else {
        switch (code) {
        case 0:
            return red0;
        case 1:
            return red1;
        case 2:
            return (4 * red0 + 1 * red1) / 5;
        case 3:
            return (3 * red0 + 2 * red1) / 5;
        case 4:
            return (2 * red0 + 3 * red1) / 5;
        case 5:
            return (1 * red0 + 4 * red1) / 5;
        case 6:
            return 0;
        case 7:
            return 0xff;
        }
    }
    return 0;
}

void main() {
    uvec2 packed_bits = imageLoad(bc4_input, ivec3(gl_WorkGroupID + src_offset)).rg;
    uint64_t bits = packUint2x32(packed_bits);
    uint red = DecompressBlock(bits, gl_LocalInvocationID.xy);
    uvec4 color = uvec4(red & 0xff, 0, 0, 0xff);
    imageStore(bc4_output, ivec3(gl_GlobalInvocationID + dst_offset), color);
}
"#;

/// Upstream: `host_shaders/opengl_lmem_warmup.comp`
pub const OPENGL_LMEM_WARMUP_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

// This shader is a workaround for a quirk in NVIDIA OpenGL drivers
// Shaders using local memory see a great performance benefit if a shader that was dispatched
// before it had more local memory allocated.
// This shader allocates the maximum local memory allowed on NVIDIA drivers to ensure that
// subsequent shaders see the performance boost.

// NOTE: This shader does no actual meaningful work and returns immediately,
// it is simply a means to have the driver expect a shader using lots of local memory.

#version 450

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(location = 0) uniform uint uniform_data;

layout(binding = 0, rgba8) uniform writeonly restrict image2DArray dest_image;

#define MAX_LMEM_SIZE 4080 // Size chosen to avoid errors in Nvidia's GLSL compiler
#define NUM_LMEM_CONSTANTS 1
#define ARRAY_SIZE MAX_LMEM_SIZE - NUM_LMEM_CONSTANTS

uint lmem_0[ARRAY_SIZE];
const uvec4 constant_values[NUM_LMEM_CONSTANTS] = uvec4[](uvec4(0));

void main() {
    const uint global_id = gl_GlobalInvocationID.x;
    if (global_id <= 128) {
        // Since the shader is called with a dispatch of 1x1x1
        // This should always be the case, and this shader will not actually execute
        return;
    }
    for (uint t = 0; t < uniform_data; t++) {
        const uint offset = (t * uniform_data);
        lmem_0[offset] = t;
    }
    const uint offset = (gl_GlobalInvocationID.y * uniform_data + gl_GlobalInvocationID.x);
    const uint value = lmem_0[offset];
    const uint const_value = constant_values[offset / 4][offset % 4];
    const uvec4 color = uvec4(value + const_value);

    // A "side-effect" is needed so the variables don't get optimized out,
    // but this should never execute so there should be no clobbering of previously bound state.
    imageStore(dest_image, ivec3(gl_GlobalInvocationID), color);
}
"#;

/// Upstream: `host_shaders/pitch_unswizzle.comp`
pub const PITCH_UNSWIZZLE_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 430

#ifdef VULKAN

#extension GL_EXT_shader_16bit_storage : require
#extension GL_EXT_shader_8bit_storage : require
#define HAS_EXTENDED_TYPES 1
#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 1

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#extension GL_NV_gpu_shader5 : enable
#ifdef GL_NV_gpu_shader5
#define HAS_EXTENDED_TYPES 1
#else
#define HAS_EXTENDED_TYPES 0
#endif
#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define UNIFORM(n) layout (location = n) uniform
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 0

#endif

BEGIN_PUSH_CONSTANTS
UNIFORM(0) uvec2 origin;
UNIFORM(1) ivec2 destination;
UNIFORM(2) uint bytes_per_block;
UNIFORM(3) uint pitch;
END_PUSH_CONSTANTS

#if HAS_EXTENDED_TYPES
layout(binding = BINDING_INPUT_BUFFER, std430) readonly buffer InputBufferU8 { uint8_t u8data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) readonly buffer InputBufferU16 { uint16_t u16data[]; };
#endif
layout(binding = BINDING_INPUT_BUFFER, std430) readonly buffer InputBufferU32 { uint u32data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) readonly buffer InputBufferU64 { uvec2 u64data[]; };
layout(binding = BINDING_INPUT_BUFFER, std430) readonly buffer InputBufferU128 { uvec4 u128data[]; };

layout(binding = BINDING_OUTPUT_IMAGE) writeonly uniform uimage2D output_image;

layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;

uvec4 ReadTexel(uint offset) {
    switch (bytes_per_block) {
#if HAS_EXTENDED_TYPES
    case 1:
        return uvec4(u8data[offset], 0, 0, 0);
    case 2:
        return uvec4(u16data[offset / 2], 0, 0, 0);
#else
    case 1:
        return uvec4(bitfieldExtract(u32data[offset / 4], int((offset * 8) & 24), 8), 0, 0, 0);
    case 2:
        return uvec4(bitfieldExtract(u32data[offset / 4], int((offset * 8) & 16), 16), 0, 0, 0);
#endif
    case 4:
        return uvec4(u32data[offset / 4], 0, 0, 0);
    case 8:
        return uvec4(u64data[offset / 8], 0, 0);
    case 16:
        return u128data[offset / 16];
    }
    return uvec4(0);
}

void main() {
    uvec2 pos = gl_GlobalInvocationID.xy + origin;

    uint offset = 0;
    offset += pos.x * bytes_per_block;
    offset += pos.y * pitch;

    const uvec4 texel = ReadTexel(offset);
    const ivec2 coord = ivec2(gl_GlobalInvocationID.xy) + destination;
    imageStore(output_image, coord, texel);
}
"#;

/// Upstream: `host_shaders/queries_prefix_scan_sum.comp`
pub const QUERIES_PREFIX_SCAN_SUM_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

#version 460 core

#extension GL_KHR_shader_subgroup_basic : require
#extension GL_KHR_shader_subgroup_shuffle : require
#extension GL_KHR_shader_subgroup_shuffle_relative : require
#extension GL_KHR_shader_subgroup_arithmetic : require

#ifdef VULKAN

#define HAS_EXTENDED_TYPES 1
#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 1

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#extension GL_NV_gpu_shader5 : enable
#ifdef GL_NV_gpu_shader5
#define HAS_EXTENDED_TYPES 1
#else
#define HAS_EXTENDED_TYPES 0
#endif
#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define UNIFORM(n) layout(location = n) uniform
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 0

#endif

BEGIN_PUSH_CONSTANTS
UNIFORM(0) uint min_accumulation_base;
UNIFORM(1) uint max_accumulation_base;
UNIFORM(2) uint accumulation_limit;
UNIFORM(3) uint buffer_offset;
END_PUSH_CONSTANTS

#define LOCAL_RESULTS 8
#define QUERIES_PER_INVOC 2048

layout(local_size_x = QUERIES_PER_INVOC / LOCAL_RESULTS) in;

layout(std430, binding = 0) readonly buffer block1 {
    uvec2 input_data[];
};

layout(std430, binding = 1) coherent buffer block2 {
    uvec2 output_data[];
};

layout(std430, binding = 2) coherent buffer block3 {
    uvec2 accumulated_data;
};

shared uvec2 shared_data[128];

// Simple Uint64 add that uses 2 uint variables for GPUs that don't support uint64
uvec2 AddUint64(uvec2 value_1, uvec2 value_2) {
    uint carry = 0;
    uvec2 result;
    result.x = uaddCarry(value_1.x, value_2.x, carry);
    result.y = value_1.y + value_2.y + carry;
    return result;
}

// do subgroup Prefix Sum using Hillis and Steele's algorithm
uvec2 subgroupInclusiveAddUint64(uvec2 value) {
    uvec2 result = value;
    for (uint i = 1; i < gl_SubgroupSize; i *= 2) {
        uvec2 other = subgroupShuffleUp(result, i); // get value from subgroup_inv_id - i;
        if (i <= gl_SubgroupInvocationID) {
            result = AddUint64(result, other);
        }
    }
    return result;
}

// Writes down the results to the output buffer and to the accumulation buffer
void WriteResults(uvec2 results[LOCAL_RESULTS]) {
    const uint current_id = gl_LocalInvocationID.x;
    const uvec2 accum = accumulated_data;
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        uvec2 base_data = current_id * LOCAL_RESULTS + i < min_accumulation_base ? accum : uvec2(0, 0);
        AddUint64(results[i], base_data);
    }
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        output_data[buffer_offset + current_id * LOCAL_RESULTS + i] = results[i];
    }
    uint index = accumulation_limit % LOCAL_RESULTS;
    uint base_id = accumulation_limit / LOCAL_RESULTS;
    if (min_accumulation_base >= accumulation_limit + 1) {
        if (current_id == base_id) {
            accumulated_data = results[index];
        }
        return;
    }
    // We have that ugly case in which the accumulation data is reset in the middle somewhere.
    barrier();
    groupMemoryBarrier();

    if (current_id == base_id) {
        uvec2 reset_value = output_data[max_accumulation_base - 1];
        // Calculate two complement / negate manually
        reset_value = AddUint64(uvec2(1,0), ~reset_value);
        accumulated_data = AddUint64(results[index], reset_value);
    }
}

void main() {
    const uint subgroup_inv_id = gl_SubgroupInvocationID;
    const uint subgroup_id = gl_SubgroupID + gl_WorkGroupID.x * gl_NumSubgroups;
    const uint last_subgroup_id = subgroupMax(subgroup_inv_id);
    const uint current_id = gl_LocalInvocationID.x;
    const uint total_work = accumulation_limit;
    const uint last_result_id = LOCAL_RESULTS - 1;
    uvec2 data[LOCAL_RESULTS];
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        data[i] = input_data[buffer_offset + current_id * LOCAL_RESULTS + i];
    }
    uvec2 results[LOCAL_RESULTS];
    results[0] = data[0];
    for (uint i = 1; i < LOCAL_RESULTS; i++) {
        results[i] = AddUint64(data[i], results[i - 1]);
    }
    // make sure all input data has been loaded
    subgroupBarrier();
    subgroupMemoryBarrier();

    // on the last local result, do a subgroup inclusive scan sum
    results[last_result_id] = subgroupInclusiveAddUint64(results[last_result_id]);
    // get the last local result from the subgroup behind the current
    uvec2 result_behind = subgroupShuffleUp(results[last_result_id], 1);
    if (subgroup_inv_id != 0) {
        for (uint i = 1; i < LOCAL_RESULTS; i++) {
            results[i - 1] = AddUint64(results[i - 1], result_behind);
        }
    }

    // if we had less queries than our subgroup, just write down the results.
    if (total_work <= gl_SubgroupSize * LOCAL_RESULTS) { // This condition is constant per dispatch.
        WriteResults(results);
        return;
    }

    // We now have more, so lets write the last result into shared memory.
    // Only pick the last subgroup.
    if (subgroup_inv_id == last_subgroup_id) {
        shared_data[subgroup_id] = results[last_result_id];
    }
    // wait until everyone loaded their stuffs
    barrier();
    memoryBarrierShared();

    // only if it's not the first subgroup
    if (subgroup_id != 0) {
        // get the results from some previous invocation
        uvec2 tmp = shared_data[subgroup_inv_id];
        subgroupBarrier();
        subgroupMemoryBarrierShared();
        tmp = subgroupInclusiveAddUint64(tmp);
        // obtain the result that would be equivalent to the previous result
        uvec2 shuffled_result = subgroupShuffle(tmp, subgroup_id - 1);
        for (uint i = 0; i < LOCAL_RESULTS; i++) {
            results[i] = AddUint64(results[i], shuffled_result);
        }
    }
    WriteResults(results);
}"#;

/// Upstream: `host_shaders/queries_prefix_scan_sum_nosubgroups.comp`
pub const QUERIES_PREFIX_SCAN_SUM_NOSUBGROUPS_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2015 Graham Sellers, Richard Wright Jr. and Nicholas Haemel
// SPDX-License-Identifier: MIT

// Code obtained from OpenGL SuperBible, Seventh Edition by Graham Sellers, Richard Wright Jr. and
// Nicholas Haemel. Modified to suit needs.

#version 460 core

#ifdef VULKAN

#define HAS_EXTENDED_TYPES 1
#define BEGIN_PUSH_CONSTANTS layout(push_constant) uniform PushConstants {
#define END_PUSH_CONSTANTS };
#define UNIFORM(n)
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 1

#else // ^^^ Vulkan ^^^ // vvv OpenGL vvv

#extension GL_NV_gpu_shader5 : enable
#ifdef GL_NV_gpu_shader5
#define HAS_EXTENDED_TYPES 1
#else
#define HAS_EXTENDED_TYPES 0
#endif
#define BEGIN_PUSH_CONSTANTS
#define END_PUSH_CONSTANTS
#define UNIFORM(n) layout(location = n) uniform
#define BINDING_INPUT_BUFFER 0
#define BINDING_OUTPUT_IMAGE 0

#endif

BEGIN_PUSH_CONSTANTS
UNIFORM(0) uint min_accumulation_base;
UNIFORM(1) uint max_accumulation_base;
UNIFORM(2) uint accumulation_limit;
UNIFORM(3) uint buffer_offset;
END_PUSH_CONSTANTS

#define LOCAL_RESULTS 4
#define QUERIES_PER_INVOC 2048

layout(local_size_x = QUERIES_PER_INVOC / LOCAL_RESULTS) in;

layout(std430, binding = 0) readonly buffer block1 {
    uvec2 input_data[gl_WorkGroupSize.x * LOCAL_RESULTS];
};

layout(std430, binding = 1) writeonly coherent buffer block2 {
    uvec2 output_data[gl_WorkGroupSize.x * LOCAL_RESULTS];
};

layout(std430, binding = 2) coherent buffer block3 {
    uvec2 accumulated_data;
};

shared uvec2 shared_data[gl_WorkGroupSize.x * LOCAL_RESULTS];

uvec2 AddUint64(uvec2 value_1, uvec2 value_2) {
    uint carry = 0;
    uvec2 result;
    result.x = uaddCarry(value_1.x, value_2.x, carry);
    result.y = value_1.y + value_2.y + carry;
    return result;
}

void main(void) {
    uint id = gl_LocalInvocationID.x;
    uvec2 base_value[LOCAL_RESULTS];
    const uvec2 accum = accumulated_data;
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        base_value[i] = (buffer_offset + id * LOCAL_RESULTS + i) < min_accumulation_base
                            ? accumulated_data
                            : uvec2(0);
    }
    uint work_size = gl_WorkGroupSize.x;
    uint rd_id;
    uint wr_id;
    uint mask;
    uvec2 inputs[LOCAL_RESULTS];
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        inputs[i] = input_data[buffer_offset + id * LOCAL_RESULTS + i];
    }
    // The number of steps is the log base 2 of the
    // work group size, which should be a power of 2
    const uint steps = uint(log2(work_size)) + uint(log2(LOCAL_RESULTS));
    uint step = 0;

    // Each invocation is responsible for the content of
    // two elements of the output array
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        shared_data[id * LOCAL_RESULTS + i] = inputs[i];
    }
    // Synchronize to make sure that everyone has initialized
    // their elements of shared_data[] with data loaded from
    // the input arrays
    barrier();
    memoryBarrierShared();
    // For each step...
    for (step = 0; step < steps; step++) {
        // Calculate the read and write index in the
        // shared array
        mask = (1 << step) - 1;
        rd_id = ((id >> step) << (step + 1)) + mask;
        wr_id = rd_id + 1 + (id & mask);
        // Accumulate the read data into our element

        shared_data[wr_id] = AddUint64(shared_data[rd_id], shared_data[wr_id]);
        // Synchronize again to make sure that everyone
        // has caught up with us
        barrier();
        memoryBarrierShared();
    }
    // Add the accumulation
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        shared_data[id * LOCAL_RESULTS + i] =
            AddUint64(shared_data[id * LOCAL_RESULTS + i], base_value[i]);
    }
    barrier();
    memoryBarrierShared();

    // Finally write our data back to the output buffer
    for (uint i = 0; i < LOCAL_RESULTS; i++) {
        output_data[buffer_offset + id * LOCAL_RESULTS + i] = shared_data[id * LOCAL_RESULTS + i];
    }
    if (id == 0) {
        if (min_accumulation_base >= accumulation_limit + 1) {
            accumulated_data = shared_data[accumulation_limit];
            return;
        }
        uvec2 reset_value = shared_data[max_accumulation_base - 1];
        uvec2 final_value = shared_data[accumulation_limit];
        // Two complements
        reset_value = AddUint64(uvec2(1, 0), ~reset_value);
        accumulated_data = AddUint64(final_value, reset_value);
    }
}"#;

/// Upstream: `host_shaders/resolve_conditional_render.comp`
pub const RESOLVE_CONDITIONAL_RENDER_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

#version 450

layout(local_size_x = 1) in;

layout(std430, binding = 0) buffer Query {
    uvec2 initial;
    uvec2 unknown;
    uvec2 current;
};

layout(std430, binding = 1) buffer Result {
    uint result;
};

void main() {
    result = all(equal(initial, current)) ? 1 : 0;
}
"#;

/// Upstream: `host_shaders/vulkan_quad_indexed.comp`
pub const VULKAN_QUAD_INDEXED_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

layout (local_size_x = 1024) in;

layout (std430, set = 0, binding = 0) readonly buffer InputBuffer {
    uint input_indexes[];
};

layout (std430, set = 0, binding = 1) writeonly buffer OutputBuffer {
    uint output_indexes[];
};

layout (push_constant) uniform PushConstants {
    uint base_vertex;
    int index_shift; // 0: uint8, 1: uint16, 2: uint32
    int is_strip; // 0: quads 1: quadstrip
};

void main() {
    int primitive = int(gl_GlobalInvocationID.x);
    if (primitive * 6 >= output_indexes.length()) {
        return;
    }

    int index_size = 8 << index_shift;
    int flipped_shift = 2 - index_shift;
    int mask = (1 << flipped_shift) - 1;

    const int quads_swizzle[6] = int[](0, 1, 2, 0, 2, 3);
    const int quad_strip_swizzle[6] = int[](0, 3, 1, 0, 2, 3);
    for (uint vertex = 0; vertex < 6; ++vertex) {
        int offset = (is_strip == 0 ? primitive * 4 + quads_swizzle[vertex] : primitive * 2 + quad_strip_swizzle[vertex]);
        int int_offset = offset >> flipped_shift;
        int bit_offset = (offset & mask) * index_size;
        uint packed_input = input_indexes[int_offset];
        uint index = bitfieldExtract(packed_input, bit_offset, index_size);
        output_indexes[primitive * 6 + vertex] = index + base_vertex;
    }
}
"#;

/// Upstream: `host_shaders/vulkan_turbo_mode.comp`
pub const VULKAN_TURBO_MODE_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core

layout (local_size_x = 16, local_size_y = 8, local_size_z = 1) in;

layout (binding = 0) buffer ThreadData {
    uint data[];
};

uint xorshift32(uint x) {
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return x;
}

uint getGlobalIndex() {
    return gl_GlobalInvocationID.x + gl_GlobalInvocationID.y * gl_WorkGroupSize.y * gl_NumWorkGroups.y;
}

void main() {
    uint myIndex = xorshift32(getGlobalIndex());
    uint otherIndex = xorshift32(myIndex);

    uint otherValue = atomicAdd(data[otherIndex % data.length()], 0) + 1;
    atomicAdd(data[myIndex % data.length()], otherValue);
}
"#;

/// Upstream: `host_shaders/vulkan_uint8.comp`
pub const VULKAN_UINT8_COMP: &str = r#"// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#version 460 core
#extension GL_EXT_shader_16bit_storage : require
#extension GL_EXT_shader_8bit_storage : require

layout (local_size_x = 1024) in;

layout (std430, set = 0, binding = 0) readonly buffer InputBuffer {
    uint8_t input_indexes[];
};

layout (std430, set = 0, binding = 1) writeonly buffer OutputBuffer {
    uint16_t output_indexes[];
};

uint AssembleIndex(uint id) {
    // Most primitive restart indices are 0xFF
    // Hardcode this to 0xFF for now
    uint index = uint(input_indexes[id]);
    return index == 0xFF ? 0xFFFF : index;
}

void main() {
    uint id = gl_GlobalInvocationID.x;
    if (id < input_indexes.length()) {
        output_indexes[id] = uint16_t(AssembleIndex(id));
    }
}
"#;

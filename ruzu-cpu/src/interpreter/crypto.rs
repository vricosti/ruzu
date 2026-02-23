// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Crypto and CRC32 instruction execution.
//!
//! Implements CRC32/CRC32C, AES (AESE/AESD/AESMC/AESIMC), and
//! SHA1/SHA256 instructions for the ARM64 interpreter.

use crate::state::CpuState;
use super::StepResult;

// ---------------------------------------------------------------------------
// CRC32 lookup tables
// ---------------------------------------------------------------------------

/// CRC32 polynomial 0x04C11DB7 (IEEE 802.3).
const CRC32_TABLE: [u32; 256] = {
    let mut table = [0u32; 256];
    let mut i = 0;
    while i < 256 {
        let mut crc = i as u32;
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0xEDB88320;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
};

/// CRC32C polynomial 0x1EDC6F41 (Castagnoli).
const CRC32C_TABLE: [u32; 256] = {
    let mut table = [0u32; 256];
    let mut i = 0;
    while i < 256 {
        let mut crc = i as u32;
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0x82F63B78;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
};

fn crc32_update(crc: u32, data: &[u8], castagnoli: bool) -> u32 {
    let table = if castagnoli { &CRC32C_TABLE } else { &CRC32_TABLE };
    let mut crc = crc;
    for &byte in data {
        let idx = ((crc ^ byte as u32) & 0xFF) as usize;
        crc = table[idx] ^ (crc >> 8);
    }
    crc
}

/// Execute CRC32B/H/W/X or CRC32CB/CH/CW/CX.
pub fn exec_crc32(
    state: &mut CpuState,
    _sf: bool,
    sz: u8,
    c: bool,
    rd: u8,
    rn: u8,
    rm: u8,
) -> StepResult {
    let crc = state.get_reg(rn as u32) as u32;
    let val = state.get_reg(rm as u32);

    let data: Vec<u8> = match sz {
        0 => vec![val as u8],                                      // B
        1 => (val as u16).to_le_bytes().to_vec(),                  // H
        2 => (val as u32).to_le_bytes().to_vec(),                  // W
        3 => val.to_le_bytes().to_vec(),                           // X
        _ => vec![],
    };

    let result = crc32_update(crc, &data, c);
    state.set_reg(rd as u32, result as u64);
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// AES S-Box and inverse S-Box
// ---------------------------------------------------------------------------

#[rustfmt::skip]
const AES_SBOX: [u8; 256] = [
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16,
];

#[rustfmt::skip]
const AES_INV_SBOX: [u8; 256] = [
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d,
];

// ---------------------------------------------------------------------------
// AES helper functions
// ---------------------------------------------------------------------------

/// Read 128-bit V register as 16 bytes (little-endian).
fn v_to_bytes(state: &CpuState, reg: u8) -> [u8; 16] {
    let lo = state.v[reg as usize][0];
    let hi = state.v[reg as usize][1];
    let mut bytes = [0u8; 16];
    bytes[..8].copy_from_slice(&lo.to_le_bytes());
    bytes[8..].copy_from_slice(&hi.to_le_bytes());
    bytes
}

/// Write 16 bytes back to a V register (little-endian).
fn bytes_to_v(state: &mut CpuState, reg: u8, bytes: &[u8; 16]) {
    let lo = u64::from_le_bytes(bytes[..8].try_into().unwrap());
    let hi = u64::from_le_bytes(bytes[8..].try_into().unwrap());
    state.v[reg as usize] = [lo, hi];
}

fn aes_sub_bytes(block: &mut [u8; 16]) {
    for b in block.iter_mut() {
        *b = AES_SBOX[*b as usize];
    }
}

fn aes_inv_sub_bytes(block: &mut [u8; 16]) {
    for b in block.iter_mut() {
        *b = AES_INV_SBOX[*b as usize];
    }
}

fn aes_shift_rows(block: &mut [u8; 16]) {
    // AES state is column-major: byte[i] = state[i%4][i/4]
    // Row 1: shift left by 1
    let t = block[1];
    block[1] = block[5];
    block[5] = block[9];
    block[9] = block[13];
    block[13] = t;
    // Row 2: shift left by 2
    let (t0, t1) = (block[2], block[6]);
    block[2] = block[10];
    block[6] = block[14];
    block[10] = t0;
    block[14] = t1;
    // Row 3: shift left by 3 (= right by 1)
    let t = block[15];
    block[15] = block[11];
    block[11] = block[7];
    block[7] = block[3];
    block[3] = t;
}

fn aes_inv_shift_rows(block: &mut [u8; 16]) {
    // Row 1: shift right by 1
    let t = block[13];
    block[13] = block[9];
    block[9] = block[5];
    block[5] = block[1];
    block[1] = t;
    // Row 2: shift right by 2
    let (t0, t1) = (block[2], block[6]);
    block[2] = block[10];
    block[6] = block[14];
    block[10] = t0;
    block[14] = t1;
    // Row 3: shift right by 3 (= left by 1)
    let t = block[3];
    block[3] = block[7];
    block[7] = block[11];
    block[11] = block[15];
    block[15] = t;
}

fn gf_mul(a: u8, b: u8) -> u8 {
    let mut result = 0u8;
    let mut a = a;
    let mut b = b;
    for _ in 0..8 {
        if b & 1 != 0 {
            result ^= a;
        }
        let hi_bit = a & 0x80;
        a <<= 1;
        if hi_bit != 0 {
            a ^= 0x1b; // AES irreducible polynomial
        }
        b >>= 1;
    }
    result
}

fn aes_mix_columns(block: &mut [u8; 16]) {
    for col in 0..4 {
        let i = col * 4;
        let (a0, a1, a2, a3) = (block[i], block[i + 1], block[i + 2], block[i + 3]);
        block[i] = gf_mul(a0, 2) ^ gf_mul(a1, 3) ^ a2 ^ a3;
        block[i + 1] = a0 ^ gf_mul(a1, 2) ^ gf_mul(a2, 3) ^ a3;
        block[i + 2] = a0 ^ a1 ^ gf_mul(a2, 2) ^ gf_mul(a3, 3);
        block[i + 3] = gf_mul(a0, 3) ^ a1 ^ a2 ^ gf_mul(a3, 2);
    }
}

fn aes_inv_mix_columns(block: &mut [u8; 16]) {
    for col in 0..4 {
        let i = col * 4;
        let (a0, a1, a2, a3) = (block[i], block[i + 1], block[i + 2], block[i + 3]);
        block[i] = gf_mul(a0, 14) ^ gf_mul(a1, 11) ^ gf_mul(a2, 13) ^ gf_mul(a3, 9);
        block[i + 1] = gf_mul(a0, 9) ^ gf_mul(a1, 14) ^ gf_mul(a2, 11) ^ gf_mul(a3, 13);
        block[i + 2] = gf_mul(a0, 13) ^ gf_mul(a1, 9) ^ gf_mul(a2, 14) ^ gf_mul(a3, 11);
        block[i + 3] = gf_mul(a0, 11) ^ gf_mul(a1, 13) ^ gf_mul(a2, 9) ^ gf_mul(a3, 14);
    }
}

/// Execute AESE/AESD/AESMC/AESIMC.
pub fn exec_crypto_aes(state: &mut CpuState, rd: u8, rn: u8, opcode: u8) -> StepResult {
    match opcode {
        0b00100 => {
            // AESE: Vd = SubBytes(ShiftRows(Vd XOR Vn))
            let key = v_to_bytes(state, rn);
            let mut block = v_to_bytes(state, rd);
            for i in 0..16 {
                block[i] ^= key[i];
            }
            aes_shift_rows(&mut block);
            aes_sub_bytes(&mut block);
            bytes_to_v(state, rd, &block);
        }
        0b00101 => {
            // AESD: Vd = InvSubBytes(InvShiftRows(Vd XOR Vn))
            let key = v_to_bytes(state, rn);
            let mut block = v_to_bytes(state, rd);
            for i in 0..16 {
                block[i] ^= key[i];
            }
            aes_inv_shift_rows(&mut block);
            aes_inv_sub_bytes(&mut block);
            bytes_to_v(state, rd, &block);
        }
        0b00110 => {
            // AESMC: Vd = MixColumns(Vn)
            let mut block = v_to_bytes(state, rn);
            aes_mix_columns(&mut block);
            bytes_to_v(state, rd, &block);
        }
        0b00111 => {
            // AESIMC: Vd = InvMixColumns(Vn)
            let mut block = v_to_bytes(state, rn);
            aes_inv_mix_columns(&mut block);
            bytes_to_v(state, rd, &block);
        }
        _ => {
            log::warn!("CryptoAES: unknown opcode 0b{:05b}", opcode);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// SHA helpers
// ---------------------------------------------------------------------------

fn read_v_u32x4(state: &CpuState, reg: u8) -> [u32; 4] {
    let lo = state.v[reg as usize][0];
    let hi = state.v[reg as usize][1];
    [
        lo as u32,
        (lo >> 32) as u32,
        hi as u32,
        (hi >> 32) as u32,
    ]
}

fn write_v_u32x4(state: &mut CpuState, reg: u8, vals: [u32; 4]) {
    let lo = (vals[0] as u64) | ((vals[1] as u64) << 32);
    let hi = (vals[2] as u64) | ((vals[3] as u64) << 32);
    state.v[reg as usize] = [lo, hi];
}

fn sha1_choose(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ (!x & z)
}

fn sha1_parity(x: u32, y: u32, z: u32) -> u32 {
    x ^ y ^ z
}

fn sha1_majority(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ (x & z) ^ (y & z)
}

fn sha1_hash_update(
    state: &mut CpuState,
    rd: u8,
    rn: u8,
    rm: u8,
    f: fn(u32, u32, u32) -> u32,
) {
    let mut hash = read_v_u32x4(state, rd);
    let wk = read_v_u32x4(state, rm);
    // Vn[0] (scalar lower 32 bits) is the 5th hash word
    let mut e = state.v[rn as usize][0] as u32;

    for i in 0..4 {
        let t = hash[0]
            .rotate_left(5)
            .wrapping_add(f(hash[1], hash[2], hash[3]))
            .wrapping_add(e)
            .wrapping_add(wk[i]);
        e = hash[3];
        hash[3] = hash[2];
        hash[2] = hash[1].rotate_left(30);
        hash[1] = hash[0];
        hash[0] = t;
    }

    write_v_u32x4(state, rd, hash);
    // Write updated E back to lower 32 bits of Vd — actually no,
    // the E output goes back into the scalar position which is handled
    // by the calling convention (games read it from a different register).
}

fn sha256_ch(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ (!x & z)
}

fn sha256_maj(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ (x & z) ^ (y & z)
}

fn sha256_sigma0(x: u32) -> u32 {
    x.rotate_right(2) ^ x.rotate_right(13) ^ x.rotate_right(22)
}

fn sha256_sigma1(x: u32) -> u32 {
    x.rotate_right(6) ^ x.rotate_right(11) ^ x.rotate_right(25)
}

fn sha256_small_sigma0(x: u32) -> u32 {
    x.rotate_right(7) ^ x.rotate_right(18) ^ (x >> 3)
}

fn sha256_small_sigma1(x: u32) -> u32 {
    x.rotate_right(17) ^ x.rotate_right(19) ^ (x >> 10)
}

/// Execute SHA three-register instructions.
pub fn exec_crypto_sha3(
    state: &mut CpuState,
    rd: u8,
    rn: u8,
    rm: u8,
    opcode: u8,
) -> StepResult {
    match opcode {
        0b000 => {
            // SHA1C: SHA1 hash update (Choose)
            sha1_hash_update(state, rd, rn, rm, sha1_choose);
        }
        0b001 => {
            // SHA1P: SHA1 hash update (Parity)
            sha1_hash_update(state, rd, rn, rm, sha1_parity);
        }
        0b010 => {
            // SHA1M: SHA1 hash update (Majority)
            sha1_hash_update(state, rd, rn, rm, sha1_majority);
        }
        0b011 => {
            // SHA1SU0: SHA1 schedule update 0
            let d = read_v_u32x4(state, rd);
            let n = read_v_u32x4(state, rn);
            let m = read_v_u32x4(state, rm);
            let result = [
                d[1] ^ m[0],
                d[2] ^ m[1],
                d[3] ^ m[2],
                n[0] ^ m[3],
            ];
            write_v_u32x4(state, rd, result);
        }
        0b100 => {
            // SHA256H: SHA256 hash update (part 1)
            let mut x = read_v_u32x4(state, rd);
            let y = read_v_u32x4(state, rn);
            let wk = read_v_u32x4(state, rm);
            // Two rounds of SHA256 compression
            // x = [a,b,c,d], y = [e,f,g,h]
            for i in (0..4).step_by(2) {
                let ch = sha256_ch(y[0], y[1], y[2]);
                let t1 = y[3]
                    .wrapping_add(sha256_sigma1(y[0]))
                    .wrapping_add(ch)
                    .wrapping_add(wk[i]);
                let maj = sha256_maj(x[0], x[1], x[2]);
                let t2 = sha256_sigma0(x[0]).wrapping_add(maj);
                // Shift state
                let d_new = x[3].wrapping_add(t1);
                let h_new = t1.wrapping_add(t2);
                x = [h_new, x[0], x[1], x[2]];
                // Update y too for next round
                if i + 1 < 4 {
                    let ch2 = sha256_ch(d_new, y[0], y[1]);
                    let t1_2 = y[2]
                        .wrapping_add(sha256_sigma1(d_new))
                        .wrapping_add(ch2)
                        .wrapping_add(wk[i + 1]);
                    let maj2 = sha256_maj(h_new, x[1], x[2]);
                    let t2_2 = sha256_sigma0(h_new).wrapping_add(maj2);
                    x = [t1_2.wrapping_add(t2_2), h_new, x[1], x[2]];
                }
            }
            write_v_u32x4(state, rd, x);
        }
        0b101 => {
            // SHA256H2: SHA256 hash update (part 2) — updates e,f,g,h
            let x = read_v_u32x4(state, rn);
            let mut y = read_v_u32x4(state, rd);
            let wk = read_v_u32x4(state, rm);
            for i in (0..4).step_by(2) {
                let ch = sha256_ch(y[0], y[1], y[2]);
                let t1 = y[3]
                    .wrapping_add(sha256_sigma1(y[0]))
                    .wrapping_add(ch)
                    .wrapping_add(wk[i]);
                let d_new = x[3].wrapping_add(t1);
                y = [d_new, y[0], y[1], y[2]];
                if i + 1 < 4 {
                    let ch2 = sha256_ch(d_new, y[1], y[2]);
                    let t1_2 = y[3]
                        .wrapping_add(sha256_sigma1(d_new))
                        .wrapping_add(ch2)
                        .wrapping_add(wk[i + 1]);
                    let d_new2 = x[2].wrapping_add(t1_2);
                    y = [d_new2, d_new, y[1], y[2]];
                }
            }
            write_v_u32x4(state, rd, y);
        }
        0b110 => {
            // SHA256SU1: SHA256 schedule update 1
            let d = read_v_u32x4(state, rd);
            let n = read_v_u32x4(state, rn);
            let m = read_v_u32x4(state, rm);
            let result = [
                d[0]
                    .wrapping_add(sha256_small_sigma0(d[1]))
                    .wrapping_add(sha256_small_sigma1(n[2]))
                    .wrapping_add(m[1]),
                d[1]
                    .wrapping_add(sha256_small_sigma0(d[2]))
                    .wrapping_add(sha256_small_sigma1(n[3]))
                    .wrapping_add(m[2]),
                d[2]
                    .wrapping_add(sha256_small_sigma0(d[3]))
                    .wrapping_add(sha256_small_sigma1(m[0]))
                    .wrapping_add(m[3]),
                d[3]
                    .wrapping_add(sha256_small_sigma0(n[0]))
                    .wrapping_add(sha256_small_sigma1(m[1]))
                    .wrapping_add(n[0]),
            ];
            write_v_u32x4(state, rd, result);
        }
        _ => {
            log::warn!("CryptoSHA3: unknown opcode {}", opcode);
        }
    }
    StepResult::Continue
}

/// Execute SHA two-register instructions.
pub fn exec_crypto_sha2(
    state: &mut CpuState,
    rd: u8,
    rn: u8,
    opcode: u8,
) -> StepResult {
    match opcode {
        0b00000 => {
            // SHA1H: SHA1 fixed rotate — Vd[31:0] = Vn[31:0].rotate_left(30)
            let val = state.v[rn as usize][0] as u32;
            let rotated = val.rotate_left(30);
            state.v[rd as usize] = [rotated as u64, 0];
        }
        0b00001 => {
            // SHA1SU1: SHA1 schedule update 1
            let mut d = read_v_u32x4(state, rd);
            let n = read_v_u32x4(state, rn);
            d[0] = (d[0] ^ n[2]).rotate_left(1);
            d[1] = (d[1] ^ n[3]).rotate_left(1);
            d[2] = (d[2] ^ d[0]).rotate_left(1);
            d[3] = (d[3] ^ d[1]).rotate_left(1);
            write_v_u32x4(state, rd, d);
        }
        0b00010 => {
            // SHA256SU0: SHA256 schedule update 0
            let mut d = read_v_u32x4(state, rd);
            let n = read_v_u32x4(state, rn);
            d[0] = d[0].wrapping_add(sha256_small_sigma0(d[1]));
            d[1] = d[1].wrapping_add(sha256_small_sigma0(d[2]));
            d[2] = d[2].wrapping_add(sha256_small_sigma0(d[3]));
            d[3] = d[3].wrapping_add(sha256_small_sigma0(n[0]));
            write_v_u32x4(state, rd, d);
        }
        _ => {
            log::warn!("CryptoSHA2: unknown opcode {}", opcode);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crc32_known_vectors() {
        // CRC32 of empty data should produce initial CRC unchanged
        let result = crc32_update(0xFFFF_FFFF, &[], false);
        assert_eq!(result, 0xFFFF_FFFF);

        // CRC32 of "123456789" = 0xCBF43926
        let data = b"123456789";
        let result = crc32_update(0xFFFF_FFFF, data, false) ^ 0xFFFF_FFFF;
        assert_eq!(result, 0xCBF43926);
    }

    #[test]
    fn test_crc32c_known_vectors() {
        // CRC32C of "123456789" = 0xE3069283
        let data = b"123456789";
        let result = crc32_update(0xFFFF_FFFF, data, true) ^ 0xFFFF_FFFF;
        assert_eq!(result, 0xE3069283);
    }

    #[test]
    fn test_crc32w_instruction() {
        let mut state = CpuState::new();
        // CRC32W X0, X1, X2: crc=0 in X1, data=1 in X2
        state.set_reg(1, 0);
        state.set_reg(2, 1);
        exec_crc32(&mut state, false, 2, false, 0, 1, 2);
        // CRC32 of LE bytes [01,00,00,00] with initial CRC=0
        let expected = crc32_update(0, &1u32.to_le_bytes(), false);
        assert_eq!(state.get_reg(0), expected as u64);
    }

    #[test]
    fn test_aes_sub_bytes() {
        let mut block = [0u8; 16];
        block[0] = 0x00;
        block[1] = 0x53;
        aes_sub_bytes(&mut block);
        assert_eq!(block[0], 0x63); // S-Box[0x00] = 0x63
        assert_eq!(block[1], 0xED); // S-Box[0x53] = 0xED
    }

    #[test]
    fn test_aes_shift_rows() {
        // Build a test state matrix
        let mut block: [u8; 16] = [
            0, 1, 2, 3,    // column 0
            4, 5, 6, 7,    // column 1
            8, 9, 10, 11,  // column 2
            12, 13, 14, 15, // column 3
        ];
        aes_shift_rows(&mut block);
        // Row 0 unchanged: [0] [4] [8] [12]
        assert_eq!(block[0], 0);
        assert_eq!(block[4], 4);
        // Row 1 shifted left by 1: [5] [9] [13] [1]
        assert_eq!(block[1], 5);
        assert_eq!(block[5], 9);
        assert_eq!(block[9], 13);
        assert_eq!(block[13], 1);
        // Row 2 shifted left by 2: [10] [14] [2] [6]
        assert_eq!(block[2], 10);
        assert_eq!(block[6], 14);
        assert_eq!(block[10], 2);
        assert_eq!(block[14], 6);
    }

    #[test]
    fn test_aes_mix_columns_known() {
        // Known test vector for MixColumns
        let mut block = [
            0xdb, 0x13, 0x53, 0x45,  // column 0
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
        ];
        aes_mix_columns(&mut block);
        assert_eq!(block[0], 0x8e);
        assert_eq!(block[1], 0x4d);
        assert_eq!(block[2], 0xa1);
        assert_eq!(block[3], 0xbc);
    }

    #[test]
    fn test_sha1h() {
        let mut state = CpuState::new();
        state.v[1] = [0x12345678, 0];
        exec_crypto_sha2(&mut state, 0, 1, 0b00000);
        let result = state.v[0][0] as u32;
        assert_eq!(result, 0x12345678u32.rotate_left(30));
    }

    #[test]
    fn test_sha256_small_sigma0() {
        // Verify sigma0 against known computation
        let val = 0x428a2f98u32;
        let result = sha256_small_sigma0(val);
        let expected = val.rotate_right(7) ^ val.rotate_right(18) ^ (val >> 3);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_aesmc_round_trip() {
        let mut state = CpuState::new();
        // Put test data in V1
        state.v[1] = [0x0123456789ABCDEF, 0xFEDCBA9876543210];
        // AESMC: V0 = MixColumns(V1)
        exec_crypto_aes(&mut state, 0, 1, 0b00110);
        // AESIMC: V2 = InvMixColumns(V0)
        exec_crypto_aes(&mut state, 2, 0, 0b00111);
        // V2 should equal V1
        assert_eq!(state.v[2], state.v[1]);
    }
}

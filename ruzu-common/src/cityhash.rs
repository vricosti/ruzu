//! Port of zuyu/src/common/cityhash.h and zuyu/src/common/cityhash.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! CityHash, by Geoff Pike and Jyrki Alakuijala
//! http://code.google.com/p/cityhash/

// Some primes between 2^63 and 2^64 for various uses.
const K0: u64 = 0xc3a5c85c97cb3127;
const K1: u64 = 0xb492b66fbe98f273;
const K2: u64 = 0x9ae16a3b2f90404f;

#[inline]
fn fetch64(buf: &[u8]) -> u64 {
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&buf[..8]);
    u64::from_le_bytes(bytes)
}

#[inline]
fn fetch32(buf: &[u8]) -> u64 {
    let mut bytes = [0u8; 4];
    bytes.copy_from_slice(&buf[..4]);
    u32::from_le_bytes(bytes) as u64
}

#[inline]
fn rotate(val: u64, shift: u32) -> u64 {
    if shift == 0 {
        val
    } else {
        val.rotate_right(shift)
    }
}

#[inline]
fn shift_mix(val: u64) -> u64 {
    val ^ (val >> 47)
}

/// Hash 128 input bits down to 64 bits of output.
/// This is intended to be a reasonably good hash function.
#[inline]
pub fn hash128to64(x: [u64; 2]) -> u64 {
    // Murmur-inspired hashing.
    let mul: u64 = 0x9ddfea08eb382d69;
    let mut a = (x[0] ^ x[1]).wrapping_mul(mul);
    a ^= a >> 47;
    let mut b = (x[1] ^ a).wrapping_mul(mul);
    b ^= b >> 47;
    b = b.wrapping_mul(mul);
    b
}

#[inline]
fn hash_len16(u: u64, v: u64) -> u64 {
    hash128to64([u, v])
}

#[inline]
fn hash_len16_mul(u: u64, v: u64, mul: u64) -> u64 {
    let mut a = (u ^ v).wrapping_mul(mul);
    a ^= a >> 47;
    let mut b = (v ^ a).wrapping_mul(mul);
    b ^= b >> 47;
    b = b.wrapping_mul(mul);
    b
}

fn hash_len0to16(s: &[u8]) -> u64 {
    let len = s.len();
    if len >= 8 {
        let mul = K2.wrapping_add((len as u64) * 2);
        let a = fetch64(s).wrapping_add(K2);
        let b = fetch64(&s[len - 8..]);
        let c = rotate(b, 37).wrapping_mul(mul).wrapping_add(a);
        let d = (rotate(a, 25).wrapping_add(b)).wrapping_mul(mul);
        return hash_len16_mul(c, d, mul);
    }
    if len >= 4 {
        let mul = K2.wrapping_add((len as u64) * 2);
        let a = fetch32(s);
        return hash_len16_mul((len as u64).wrapping_add(a << 3), fetch32(&s[len - 4..]), mul);
    }
    if len > 0 {
        let a = s[0] as u64;
        let b = s[len >> 1] as u64;
        let c = s[len - 1] as u64;
        let y = a.wrapping_add(b << 8) as u32;
        let z = (len as u32).wrapping_add((c as u32) << 2);
        return shift_mix(
            (y as u64).wrapping_mul(K2) ^ (z as u64).wrapping_mul(K0),
        )
        .wrapping_mul(K2);
    }
    K2
}

fn hash_len17to32(s: &[u8]) -> u64 {
    let len = s.len();
    let mul = K2.wrapping_add((len as u64) * 2);
    let a = fetch64(s).wrapping_mul(K1);
    let b = fetch64(&s[8..]);
    let c = fetch64(&s[len - 8..]).wrapping_mul(mul);
    let d = fetch64(&s[len - 16..]).wrapping_mul(K2);
    hash_len16_mul(
        rotate(a.wrapping_add(b), 43)
            .wrapping_add(rotate(c, 30))
            .wrapping_add(d),
        a.wrapping_add(rotate(b.wrapping_add(K2), 18)).wrapping_add(c),
        mul,
    )
}

fn weak_hash_len32_with_seeds(w: u64, x: u64, y: u64, z: u64, mut a: u64, mut b: u64) -> (u64, u64) {
    a = a.wrapping_add(w);
    b = rotate(b.wrapping_add(a).wrapping_add(z), 21);
    let c = a;
    a = a.wrapping_add(x);
    a = a.wrapping_add(y);
    b = b.wrapping_add(rotate(a, 44));
    (a.wrapping_add(z), b.wrapping_add(c))
}

fn weak_hash_len32_with_seeds_from_buf(s: &[u8], a: u64, b: u64) -> (u64, u64) {
    weak_hash_len32_with_seeds(
        fetch64(s),
        fetch64(&s[8..]),
        fetch64(&s[16..]),
        fetch64(&s[24..]),
        a,
        b,
    )
}

fn hash_len33to64(s: &[u8]) -> u64 {
    let len = s.len();
    let mul = K2.wrapping_add((len as u64) * 2);
    let a = fetch64(s).wrapping_mul(K2);
    let b = fetch64(&s[8..]);
    let c = fetch64(&s[len - 24..]);
    let d = fetch64(&s[len - 32..]);
    let e = fetch64(&s[16..]).wrapping_mul(K2);
    let f = fetch64(&s[24..]).wrapping_mul(9);
    let g = fetch64(&s[len - 8..]);
    let h = fetch64(&s[len - 16..]).wrapping_mul(mul);
    let u = rotate(a.wrapping_add(g), 43)
        .wrapping_add(rotate(b, 30).wrapping_add(c).wrapping_mul(9));
    let v = ((a.wrapping_add(g)) ^ d).wrapping_add(f).wrapping_add(1);
    let w = (u.wrapping_add(v)).wrapping_mul(mul).swap_bytes().wrapping_add(h);
    let x = rotate(e.wrapping_add(f), 42).wrapping_add(c);
    let y = ((v.wrapping_add(w)).wrapping_mul(mul).swap_bytes().wrapping_add(g)).wrapping_mul(mul);
    let z = e.wrapping_add(f).wrapping_add(c);
    let a2 = ((x.wrapping_add(z)).wrapping_mul(mul).wrapping_add(y))
        .swap_bytes()
        .wrapping_add(b);
    let b2 = shift_mix((z.wrapping_add(a2)).wrapping_mul(mul).wrapping_add(d).wrapping_add(h))
        .wrapping_mul(mul);
    b2.wrapping_add(x)
}

/// Hash function for a byte array (64-bit result).
pub fn city_hash64(s: &[u8]) -> u64 {
    let len = s.len();
    if len <= 32 {
        if len <= 16 {
            return hash_len0to16(s);
        } else {
            return hash_len17to32(s);
        }
    } else if len <= 64 {
        return hash_len33to64(s);
    }

    // For strings over 64 bytes we hash the end first, and then as we
    // loop we keep 56 bytes of state: v, w, x, y, and z.
    let mut x = fetch64(&s[len - 40..]);
    let mut y = fetch64(&s[len - 16..]).wrapping_add(fetch64(&s[len - 56..]));
    let mut z = hash_len16(
        fetch64(&s[len - 48..]).wrapping_add(len as u64),
        fetch64(&s[len - 24..]),
    );
    let mut v = weak_hash_len32_with_seeds_from_buf(&s[len - 64..], len as u64, z);
    let mut w = weak_hash_len32_with_seeds_from_buf(&s[len - 32..], y.wrapping_add(K1), x);
    x = x.wrapping_mul(K1).wrapping_add(fetch64(s));

    // Decrease len to the nearest multiple of 64, and operate on 64-byte chunks.
    let mut remaining = (len - 1) & !63;
    let mut offset = 0;
    loop {
        x = rotate(
            x.wrapping_add(y)
                .wrapping_add(v.0)
                .wrapping_add(fetch64(&s[offset + 8..])),
            37,
        )
        .wrapping_mul(K1);
        y = rotate(
            y.wrapping_add(v.1).wrapping_add(fetch64(&s[offset + 48..])),
            42,
        )
        .wrapping_mul(K1);
        x ^= w.1;
        y = y.wrapping_add(v.0).wrapping_add(fetch64(&s[offset + 40..]));
        z = rotate(z.wrapping_add(w.0), 33).wrapping_mul(K1);
        v = weak_hash_len32_with_seeds_from_buf(&s[offset..], v.1.wrapping_mul(K1), x.wrapping_add(w.0));
        w = weak_hash_len32_with_seeds_from_buf(
            &s[offset + 32..],
            z.wrapping_add(w.1),
            y.wrapping_add(fetch64(&s[offset + 16..])),
        );
        std::mem::swap(&mut z, &mut x);
        offset += 64;
        remaining -= 64;
        if remaining == 0 {
            break;
        }
    }

    hash_len16(
        hash_len16(v.0, w.0)
            .wrapping_add(shift_mix(y).wrapping_mul(K1))
            .wrapping_add(z),
        hash_len16(v.1, w.1).wrapping_add(x),
    )
}

/// Hash function for a byte array. For convenience, a 64-bit seed is also
/// hashed into the result.
pub fn city_hash64_with_seed(s: &[u8], seed: u64) -> u64 {
    city_hash64_with_seeds(s, K2, seed)
}

/// Hash function for a byte array. For convenience, two seeds are also
/// hashed into the result.
pub fn city_hash64_with_seeds(s: &[u8], seed0: u64, seed1: u64) -> u64 {
    hash_len16(city_hash64(s).wrapping_sub(seed0), seed1)
}

/// A subroutine for CityHash128(). Based on City and Murmur.
fn city_murmur(s: &[u8], seed: [u64; 2]) -> [u64; 2] {
    let len = s.len();
    let mut a = seed[0];
    let mut b = seed[1];
    let mut c: u64;
    let mut d: u64;
    let l = len as isize - 16;

    if l <= 0 {
        // len <= 16
        a = shift_mix(a.wrapping_mul(K1)).wrapping_mul(K1);
        c = b.wrapping_mul(K1).wrapping_add(hash_len0to16(s));
        d = shift_mix(a.wrapping_add(if len >= 8 { fetch64(s) } else { c }));
    } else {
        // len > 16
        c = hash_len16(fetch64(&s[len - 8..]).wrapping_add(K1), a);
        d = hash_len16(b.wrapping_add(len as u64), c.wrapping_add(fetch64(&s[len - 16..])));
        a = a.wrapping_add(d);
        let mut offset = 0usize;
        let mut remaining = l;
        loop {
            a ^= shift_mix(fetch64(&s[offset..]).wrapping_mul(K1)).wrapping_mul(K1);
            a = a.wrapping_mul(K1);
            b ^= a;
            c ^= shift_mix(fetch64(&s[offset + 8..]).wrapping_mul(K1)).wrapping_mul(K1);
            c = c.wrapping_mul(K1);
            d ^= c;
            offset += 16;
            remaining -= 16;
            if remaining <= 0 {
                break;
            }
        }
    }
    a = hash_len16(a, c);
    b = hash_len16(d, b);
    [a ^ b, hash_len16(b, a)]
}

/// Hash function for a byte array (128-bit result).
/// For convenience, a 128-bit seed is also hashed into the result.
pub fn city_hash128_with_seed(s: &[u8], seed: [u64; 2]) -> [u64; 2] {
    let len = s.len();
    if len < 128 {
        return city_murmur(s, seed);
    }

    // We expect len >= 128 to be the common case.
    let mut v: (u64, u64);
    let mut w: (u64, u64);
    let mut x = seed[0];
    let mut y = seed[1];
    let mut z = (len as u64).wrapping_mul(K1);

    let v_first = rotate(y ^ K1, 49).wrapping_mul(K1).wrapping_add(fetch64(s));
    let v_second = rotate(v_first, 42).wrapping_mul(K1).wrapping_add(fetch64(&s[8..]));
    v = (v_first, v_second);

    let w_first = rotate(y.wrapping_add(z), 35).wrapping_mul(K1).wrapping_add(x);
    let w_second = rotate(x.wrapping_add(fetch64(&s[88..])), 53).wrapping_mul(K1);
    w = (w_first, w_second);

    let mut offset = 0;
    let mut remaining = len;

    loop {
        // First 64-byte chunk
        x = rotate(
            x.wrapping_add(y).wrapping_add(v.0).wrapping_add(fetch64(&s[offset + 8..])),
            37,
        ).wrapping_mul(K1);
        y = rotate(
            y.wrapping_add(v.1).wrapping_add(fetch64(&s[offset + 48..])),
            42,
        ).wrapping_mul(K1);
        x ^= w.1;
        y = y.wrapping_add(v.0).wrapping_add(fetch64(&s[offset + 40..]));
        z = rotate(z.wrapping_add(w.0), 33).wrapping_mul(K1);
        v = weak_hash_len32_with_seeds_from_buf(&s[offset..], v.1.wrapping_mul(K1), x.wrapping_add(w.0));
        w = weak_hash_len32_with_seeds_from_buf(
            &s[offset + 32..],
            z.wrapping_add(w.1),
            y.wrapping_add(fetch64(&s[offset + 16..])),
        );
        std::mem::swap(&mut z, &mut x);
        offset += 64;

        // Second 64-byte chunk
        x = rotate(
            x.wrapping_add(y).wrapping_add(v.0).wrapping_add(fetch64(&s[offset + 8..])),
            37,
        ).wrapping_mul(K1);
        y = rotate(
            y.wrapping_add(v.1).wrapping_add(fetch64(&s[offset + 48..])),
            42,
        ).wrapping_mul(K1);
        x ^= w.1;
        y = y.wrapping_add(v.0).wrapping_add(fetch64(&s[offset + 40..]));
        z = rotate(z.wrapping_add(w.0), 33).wrapping_mul(K1);
        v = weak_hash_len32_with_seeds_from_buf(&s[offset..], v.1.wrapping_mul(K1), x.wrapping_add(w.0));
        w = weak_hash_len32_with_seeds_from_buf(
            &s[offset + 32..],
            z.wrapping_add(w.1),
            y.wrapping_add(fetch64(&s[offset + 16..])),
        );
        std::mem::swap(&mut z, &mut x);
        offset += 64;
        remaining -= 128;

        if remaining < 128 {
            break;
        }
    }

    x = x.wrapping_add(rotate(v.0.wrapping_add(z), 49).wrapping_mul(K0));
    y = y.wrapping_mul(K0).wrapping_add(rotate(w.1, 37));
    z = z.wrapping_mul(K0).wrapping_add(rotate(w.0, 27));
    w.0 = w.0.wrapping_mul(9);
    v.0 = v.0.wrapping_mul(K0);

    // If 0 < remaining < 128, hash up to 4 chunks of 32 bytes each from the end.
    let mut tail_done = 0;
    while tail_done < remaining {
        tail_done += 32;
        y = rotate(x.wrapping_add(y), 42).wrapping_mul(K0).wrapping_add(v.1);
        w.0 = w.0.wrapping_add(fetch64(&s[offset + remaining - tail_done + 16..]));
        x = x.wrapping_mul(K0).wrapping_add(w.0);
        z = z.wrapping_add(w.1.wrapping_add(fetch64(&s[offset + remaining - tail_done..])));
        w.1 = w.1.wrapping_add(v.0);
        v = weak_hash_len32_with_seeds_from_buf(
            &s[offset + remaining - tail_done..],
            v.0.wrapping_add(z),
            v.1,
        );
        v.0 = v.0.wrapping_mul(K0);
    }

    x = hash_len16(x, v.0);
    y = hash_len16(y.wrapping_add(z), w.0);
    [
        hash_len16(x.wrapping_add(v.1), w.1).wrapping_add(y),
        hash_len16(x.wrapping_add(w.1), y.wrapping_add(v.1)),
    ]
}

/// Hash function for a byte array (128-bit result).
pub fn city_hash128(s: &[u8]) -> [u64; 2] {
    let len = s.len();
    if len >= 16 {
        city_hash128_with_seed(&s[16..], [fetch64(s), fetch64(&s[8..]).wrapping_add(K0)])
    } else {
        city_hash128_with_seed(s, [K0, K1])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_city_hash64_empty() {
        // Just verify it doesn't panic and returns a value
        let hash = city_hash64(b"");
        assert_eq!(hash, K2); // Empty input returns K2
    }

    #[test]
    fn test_city_hash64_short() {
        let hash1 = city_hash64(b"hello");
        let hash2 = city_hash64(b"world");
        assert_ne!(hash1, hash2);
    }

    #[test]
    fn test_city_hash64_medium() {
        let hash = city_hash64(b"hello world, this is a longer string for testing!");
        assert_ne!(hash, 0);
    }

    #[test]
    fn test_city_hash64_long() {
        let data = vec![0u8; 256];
        let hash = city_hash64(&data);
        assert_ne!(hash, 0);
    }

    #[test]
    fn test_city_hash128() {
        let hash = city_hash128(b"hello world");
        assert_ne!(hash, [0, 0]);
    }

    #[test]
    fn test_hash128to64() {
        let result = hash128to64([123456789, 987654321]);
        assert_ne!(result, 0);
    }
}

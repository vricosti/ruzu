//! CRC32 implementations shared by the architecture backends.
//!
//! Rust counterpart of upstream `common/crypto/crc32.{h,cpp}`.

type Crc32Table = [u32; 256];

const fn make_table(reversed_polynomial: u32) -> Crc32Table {
    let mut table = [0u32; 256];
    let mut i = 0;
    while i < table.len() {
        let mut crc = i as u32;
        let mut bit = 0;
        while bit < 8 {
            crc = if crc & 1 != 0 {
                (crc >> 1) ^ reversed_polynomial
            } else {
                crc >> 1
            };
            bit += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
}

// Upstream's literal tables for polynomials 0x1EDC6F41 and 0x04C11DB7.
// The reflected implementation uses their reversed representations.
const CASTAGNOLI_TABLE: Crc32Table = make_table(0x82F6_3B78);
const ISO_TABLE: Crc32Table = make_table(0xEDB8_8320);

fn compute_crc32(table: &Crc32Table, mut crc: u32, value: u64, mut length: i32) -> u32 {
    let data = value.to_ne_bytes();
    let mut offset = 0usize;
    while length > 0 {
        crc = (crc >> 8) ^ table[((crc ^ u32::from(data[offset])) & 0xFF) as usize];
        offset += 1;
        length -= 1;
    }
    crc
}

/// Computes CRC32 using the Castagnoli polynomial (0x1EDC6F41).
pub extern "C" fn compute_crc32_castagnoli(crc: u32, value: u64, length: i32) -> u32 {
    compute_crc32(&CASTAGNOLI_TABLE, crc, value, length)
}

/// Computes CRC32 using the ISO polynomial (0x04C11DB7).
pub extern "C" fn compute_crc32_iso(crc: u32, value: u64, length: i32) -> u32 {
    compute_crc32(&ISO_TABLE, crc, value, length)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tables_match_upstream_boundaries() {
        assert_eq!(CASTAGNOLI_TABLE[0], 0x0000_0000);
        assert_eq!(CASTAGNOLI_TABLE[1], 0xF26B_8303);
        assert_eq!(CASTAGNOLI_TABLE[255], 0xAD7D_5351);
        assert_eq!(ISO_TABLE[0], 0x0000_0000);
        assert_eq!(ISO_TABLE[1], 0x7707_3096);
        assert_eq!(ISO_TABLE[255], 0x2D02_EF8D);
    }

    #[test]
    fn standard_check_values_match_both_polynomials() {
        let mut iso = u32::MAX;
        let mut castagnoli = u32::MAX;
        for byte in b"123456789" {
            iso = compute_crc32_iso(iso, u64::from(*byte), 1);
            castagnoli = compute_crc32_castagnoli(castagnoli, u64::from(*byte), 1);
        }
        assert_eq!(!iso, 0xCBF4_3926);
        assert_eq!(!castagnoli, 0xE306_9283);
    }

    #[test]
    fn multi_byte_value_is_consumed_from_low_to_high_address() {
        assert_eq!(
            compute_crc32_iso(u32::MAX, 0x3433_3231, 4),
            compute_crc32_iso(
                compute_crc32_iso(
                    compute_crc32_iso(compute_crc32_iso(u32::MAX, b'1' as u64, 1), b'2' as u64, 1),
                    b'3' as u64,
                    1,
                ),
                b'4' as u64,
                1,
            )
        );
    }
}

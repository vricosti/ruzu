use bitflags::bitflags;

bitflags! {
    /// x64 host capabilities used to select native emitters.
    ///
    /// Rust counterpart of upstream `backend/x64/host_feature.h`.
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct HostFeature: u64 {
        const SSSE3 = 1 << 0;
        const SSE41 = 1 << 1;
        const SSE42 = 1 << 2;
        const AVX = 1 << 3;
        const AVX2 = 1 << 4;
        const AVX512F = 1 << 5;
        const AVX512CD = 1 << 6;
        const AVX512VL = 1 << 7;
        const AVX512BW = 1 << 8;
        const AVX512DQ = 1 << 9;
        const AVX512BITALG = 1 << 10;
        const AVX512VBMI = 1 << 11;
        const PCLMULQDQ = 1 << 12;
        const F16C = 1 << 13;
        const FMA = 1 << 14;
        const AES = 1 << 15;
        const SHA = 1 << 16;
        const POPCNT = 1 << 17;
        const BMI1 = 1 << 18;
        const BMI2 = 1 << 19;
        const LZCNT = 1 << 20;
        const GFNI = 1 << 21;
        const WAITPKG = 1 << 22;

        // Zen-based BMI2. PDEP/PEXT are slow on AMD CPUs before Zen 3.
        const FAST_BMI2 = 1 << 23;

        const AVX512_ORTHO = Self::AVX512F.bits() | Self::AVX512VL.bits();
        const AVX512_ORTHO_FLOAT =
            Self::AVX512_ORTHO.bits() | Self::AVX512DQ.bits();
    }
}

#[cfg(test)]
mod tests {
    use super::HostFeature;

    #[test]
    fn aggregate_features_require_every_component() {
        let partial = HostFeature::AVX512F;
        assert!(!partial.contains(HostFeature::AVX512_ORTHO));

        let ortho = HostFeature::AVX512F | HostFeature::AVX512VL;
        assert!(ortho.contains(HostFeature::AVX512_ORTHO));
        assert!(!ortho.contains(HostFeature::AVX512_ORTHO_FLOAT));
    }
}

⏺ Finding 1 confirmed — unsigned_recip_estimate uses a "simple LUT-like approximation" and fp_recip_estimate32 just
  computes 1.0/f, neither matching ARM's 8-bit-mantissa estimate, and there's no FPSR handling. Your diagnosis is
  correct.

  My call on it: this is x64-only — on this Mac (Apple Silicon) the ARM64 backend runs, which you confirmed is correct,
  so it doesn't affect MK8D here. A faithful fix means porting ARM's RecipEstimate/RecipSqrtEstimate/Unsigned*
  reference algorithms + FP exponent handling + FPSR (~200 lines), and — critically — it can only be verified
  bit-accurately against the a32_diff oracle on an x64 host, which I can't run here. Writing it blind would violate the
  "don't claim correctness without verification" rule. So I've left it as a flagged follow-up rather than bundling an
  unverifiable numeric change into this set.

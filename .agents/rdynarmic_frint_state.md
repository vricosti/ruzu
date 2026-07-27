# Interrupted slice: A64 scalar/vector register ownership

The ARM-accurate `FPRoundInt` implementation and x64 scalar/vector emitters are
in place. Runtime verification exposed a prerequisite structural mismatch in
the A64 frontend: the Rust port used `v_scalar_read`/`v_scalar_write` for both
upstream `V(...)` vector access and `V_scalar(...)` scalar access.

Prerequisite in progress:

- Add separate `v_read`/`v_write` helpers matching upstream `V(...)`.
- Keep `v_scalar_read`/`v_scalar_write` faithful to upstream
  `V_scalar(...)` by extracting/zero-extending the low scalar element.
- Move SIMD-vector call sites to `v_read`/`v_write`, retaining scalar FP/SIMD
  call sites on `v_scalar_*`.
- Resume the scalar `FRINTX D` + `FCVTZS W` runtime verification after the
  ownership split is complete.

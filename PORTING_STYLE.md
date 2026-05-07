# PORTING_STYLE.md — Concrete Patterns For Upstream-Parity Ports

This file is the *concrete* companion to the philosophy contract in
[`CLAUDE.md`](./CLAUDE.md). Where `CLAUDE.md` says **what** to do (preserve
file structure, ownership, constants, behavior), this file shows **how**
the patterns actually look in code, derived from the conventions used on
the `refactor/sync-cell` branch (notably commit `3add328` covering
`fermi_2d`, `maxwell_3d`, `draw_manager`, `dirty_flags`, `shader_environment`
and the kernel sync-cell continuation).

When you write or modify a Rust file that ports a `.h` / `.cpp` pair, follow
these patterns unless `CLAUDE.md` or an explicit upstream behavior overrides
them.

---

## 1. File header

Every ported `.rs` file starts with the SPDX header followed by a `//!`
crate doc comment that names the upstream file(s) being ported and gives
a one-paragraph summary.

```rust
// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/fermi_2d.h and fermi_2d.cpp
//!
//! The Fermi2D engine handles 2D blits between surfaces. CallMethod and
//! CallMultiMethod stage register writes; the blit trigger consumes the
//! decoded surface descriptors and dispatches to the bound rasterizer's
//! AccelerateSurfaceCopy or to the software blit fallback.
```

For header-only ports (e.g. `shader_recompiler/environment.h`), reference
the single `.h`:

```rust
//! Port of `shader_recompiler/environment.h`
```

The license SPDX line follows the original upstream license
(`GPL-2.0-or-later` for `video_core`, `GPL-3.0-or-later` for
`shader_recompiler`, etc.). Never invent a different license.

---

## 2. Naming

| Upstream C++                          | Rust                                  |
| --------------------------------------|---------------------------------------|
| `Fermi2D::CallMethod`                 | `Fermi2D::call_method`                |
| `SetupDirtyVertexBuffers`             | `setup_dirty_vertex_buffers`          |
| `EmitA32SetCpsr`                      | `emit_a32_set_cpsr`                   |
| `enum class Operation { SrcCopy }`    | `enum Operation { SrcCopy }`          |
| `Maxwell3D::Regs::IndexFormat`        | re-export `pub use ... IndexFormat;`  |
| Free function `MaxwellToVideoCoreQuery` | `maxwell_to_video_core_query`        |

Conversion is mechanical: `CamelCase` → `snake_case` for items, keep enum
variant names verbatim. Do **not** rename at the semantic level
("ProcessLaunch" stays "process_launch"; do not turn it into "execute").

---

## 3. Constants

Constants live in the **same file as upstream**. Use `const NAME: TYPE = VALUE;`
with screaming-snake-case mirroring the upstream identifier.

For Maxwell3D-style register offsets (where `method = byte_offset / 4`),
add a brief comment explaining the convention and group by purpose with a
horizontal-line section header:

```rust
// ── Register constants (method = byte_offset / 4) ──────────────────────────

// Destination surface descriptor (0x80..0x89)
const DST_FORMAT: u32 = 0x80;
const DST_LINEAR: u32 = 0x81;
// ...
const DST_ADDR_LOW: u32 = 0x89;

// Source surface descriptor (0x8C..0x95)
const SRC_FORMAT: u32 = 0x8C;
// ...
```

Put a short doc comment on each constant whose purpose is non-obvious.
Cite upstream when the value comes from a hard-coded literal there:

```rust
/// `TryFindSize` block fetch granularity. Upstream
/// `GenericEnvironment::TryFindSize` reads 0x1000 bytes per iteration.
const TRY_FIND_SIZE_BLOCK_BYTES: usize = 0x1000;
```

Do **not** centralize constants into a sibling utility module unless
upstream itself does.

---

## 4. Enums (porting `enum class`)

Standard derive set + `#[repr(u32)]` (or whatever upstream uses) +
`#[default]` on the variant matching upstream's default-initialized value:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum Operation {
    SrcCopyAnd = 0,
    RopAnd = 1,
    Blend = 2,
    #[default]
    SrcCopy = 3,
    Rop = 4,
    SrcCopyPremult = 5,
    BlendPremult = 6,
}
```

Provide a `from_raw(raw: u32) -> Self` factory whenever upstream
reinterprets a raw register value as the enum. Default to upstream's
default variant (not a panic) for unknown values, matching upstream's
`reinterpret_cast<EnumType>(raw)` semantics:

```rust
impl PrimitiveTopologyControl {
    pub fn from_raw(raw: u32) -> Self {
        match raw {
            1 => Self::UseSeparateState,
            _ => Self::UseInBeginMethods,
        }
    }
}
```

When the same enum is used by multiple owner files, define it once in the
canonical owner (`engines/maxwell_3d.rs`) and re-export from the others:

```rust
// `PrimitiveTopology` is the upstream-faithful enum from
// `engines::maxwell_3d` (matching `Maxwell3D::Regs::PrimitiveTopology`).
// Re-exported here so existing imports of
// `engines::draw_manager::PrimitiveTopology` continue to resolve.
pub use crate::engines::maxwell_3d::PrimitiveTopology;
```

---

## 5. BitField views (porting `BitField<>` / packed registers)

Upstream uses the `BitField<offset, width, T>` macro plus an anonymous
union to overlay typed accessors on a raw u32. Rust port: a
`#[repr(transparent)]` newtype wrapping the raw word with named accessor
methods.

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
struct BlockDimensionsRaw {
    raw: u32,
}

impl BlockDimensionsRaw {
    fn block_width(self) -> u32 {
        self.raw & 0xF
    }

    fn block_height(self) -> u32 {
        (self.raw >> 4) & 0xF
    }

    fn block_depth(self) -> u32 {
        (self.raw >> 8) & 0xF
    }
}
```

Bit ranges, masks, and the meaning of "0" must match upstream byte-for-byte.
Document the upstream layout in a doc comment when the layout is non-obvious:

```rust
/// Small (packed) index buffer parameters, decoded from a single u32.
///
/// Upstream `Maxwell3D::Regs::IndexBufferSmall`:
///   bits [0:15]  = first
///   bits [16:27] = count
///   bits [28:31] = topology (PrimitiveTopology)
```

---

## 6. POD raw structs (guest memory layouts)

Anywhere a struct is read from or written to guest memory by raw byte
copy, use `#[repr(C)]` + `bytemuck::Pod` / `Zeroable`. Field order, type
sizes, and padding **must** match upstream verbatim:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(C)]
struct PixelsFromMemory {
    block_shape: BlockShapeRaw,
    corral_size: CorralSizeRaw,
    safe_overlap: SafeOverlapRaw,
    sample_mode: SampleModeRaw,
    padding: [u32; 8],
    dst_x0: i32,
    dst_y0: i32,
    dst_width: i32,
    dst_height: i32,
    du_dx: i64,
    dv_dy: i64,
    src_x0: i64,
    src_y0: i64,
}

unsafe impl Zeroable for PixelsFromMemory {}
unsafe impl Pod for PixelsFromMemory {}
```

Reserved/padding fields stay as `[u32; N]` with the exact upstream count.
Never collapse padding to a single `_pad: u32` "to clean it up".

When the raw view differs from the logical view (because Rust enums can't
hold arbitrary u32 values), keep both and add a `From<Raw> for Logical`
conversion in the same file. Example: `SurfaceRaw` (POD on the wire) vs
`Surface` (logical, with `RenderTargetFormat` enum).

---

## 7. Method ownership

Every upstream method ports to a method (or free function) **in the same
file** as upstream's `.cpp`. Each ported method gets a doc comment that
names its upstream counterpart:

```rust
/// Corresponds to upstream `Fermi2D::CallMethod`.
pub fn call_method(&mut self, method: u32, argument: u32, is_last_call: bool) {
    ...
}

/// Corresponds to upstream `Fermi2D::CallMultiMethod`.
pub fn call_multi_method(
    &mut self,
    method: u32,
    args: &[u32],
    _amount: u32,
    methods_pending: u32,
) {
    ...
}

/// Corresponds to `Fermi2D::BindRasterizer`.
pub fn bind_rasterizer(&mut self, rasterizer: &dyn RasterizerInterface) {
    ...
}
```

Free helpers that exist in upstream as static functions go at module scope
in the same file; doc-comment them with a `Port of <UpstreamFn>` header
when they are a literal port:

```rust
/// Port of `DrawManager::UpdateTopology()` as a pure owner-local helper.
///
/// This keeps the topology-resolution logic in the matching upstream owner
/// file even when other owners need the same resolved draw-state topology.
pub fn resolve_draw_topology(
    mut draw_topology: PrimitiveTopology,
    primitive_topology_control: PrimitiveTopologyControl,
    topology_override: PrimitiveTopologyOverride,
    topology_override_raw: u32,
) -> PrimitiveTopology {
    ...
}
```

**Argument count and order match upstream.** Don't add/remove/reorder
parameters. If upstream takes `(method, argument, is_last_call)`, port
takes `(method, argument, is_last_call)` — never recover `is_last_call`
from somewhere else.

---

## 8. Traits (porting virtual base classes / interfaces)

When upstream declares an abstract base in a `.h`, port it as a trait in
the matching Rust file:

```rust
//! Port of `shader_recompiler/environment.h`
//!
//! Abstract environment interface providing access to the shader binary
//! and GPU state needed during shader translation.

pub trait Environment {
    /// Read a 64-bit instruction at the given byte address.
    fn read_instruction(&mut self, address: u32) -> u64;

    /// Read a 32-bit value from a constant buffer.
    fn read_cbuf_value(&mut self, cbuf_index: u32, cbuf_offset: u32) -> u32;

    // ... one trait method per upstream pure-virtual member, in the same order
}
```

Concrete implementations (`GenericEnvironment`, `GraphicsEnvironment`,
`FileEnvironment` in upstream) go in the matching `.cpp` port file as
`impl Environment for X`. Method order mirrors upstream member order so
diffing the headers stays straightforward.

---

## 9. Pointer-to-virtual-interface adaptations

When upstream stores a raw `Interface*` member that is borrowed across
calls (rasterizer pointers, JIT callback pointers, etc.), the Rust port
stores the trait-object fat pointer as `Option<[usize; 2]>` and
reconstructs it on use. This lets owners hold a reference without
fighting Rust's borrow checker over `&dyn Trait`:

```rust
pub struct Fermi2D {
    ...
    rasterizer: Option<[usize; 2]>,
    ...
}

impl Fermi2D {
    pub fn bind_rasterizer(&mut self, rasterizer: &dyn RasterizerInterface) {
        self.rasterizer = Some(unsafe {
            std::mem::transmute::<*const dyn RasterizerInterface, [usize; 2]>(rasterizer)
        });
    }
}

// Inside execute_pending(...):
if let Some(rasterizer_raw) = self.rasterizer {
    let rasterizer = unsafe {
        std::mem::transmute::<[usize; 2], *mut dyn RasterizerInterface>(rasterizer_raw)
    };
    let rasterizer = unsafe { &mut *rasterizer };
    if rasterizer.accelerate_surface_copy(&src_surface, &dst_surface, &blit_config) {
        return vec![];
    }
}
```

Use this pattern only when upstream has the same lifetime ownership
(borrowed from a longer-lived owner). It is an *intentional* adaptation
and must be flagged in the relevant DIFF.md entry.

---

## 10. Shared-state ownership (`Arc<Mutex<...>>`)

When upstream passes a `Tegra::MemoryManager&` or `MaxwellDMA::MemoryManager&`
into a constructor, the Rust port takes `Arc<Mutex<MemoryManager>>` (or
`Arc<parking_lot::Mutex<...>>` where contention matters). Pass at
construction; clone the `Arc` for any sub-owner that needs it:

```rust
impl Fermi2D {
    pub fn new(memory_manager: Arc<Mutex<MemoryManager>>) -> Self {
        let mut this = Self {
            regs: RegsStorageRaw::default(),
            interface_state: ...,
            memory_manager: Arc::clone(&memory_manager),
            ...
            sw_blitter: SoftwareBlitEngine::new(memory_manager),
            ...
        };
        ...
    }
}
```

This matches upstream's "constructor takes a reference, member stores a
pointer" ownership boundary while satisfying Rust's send/sync rules.

---

## 11. Callback closures across owner boundaries

When upstream uses a member function pointer or `std::function` to bridge
two owners that don't otherwise reach each other (e.g. `MemoryManager`
needs to read guest CPU memory but doesn't know about `Gpu`), the Rust
port uses an `Arc<dyn Fn(...) + Send + Sync>` closure injected by the
parent at wiring time. Keep the closure shape minimal (one job, one
parameter set):

```rust
self.memory_manager
    .as_ref()
    .expect("memory_manager set before channel init")
    .lock()
    .set_guest_memory_reader(Arc::new(move |addr, output| unsafe {
        let gpu = &*(gpu_ptr as *const crate::gpu::Gpu);
        let _ = gpu.read_guest_memory(addr, output);
    }));
```

Document the upstream owner this replaces:

```rust
/// Guest CPU-memory reader callback shape: read `bytes.len()` bytes starting
/// at the given CPU address.
///
/// Rust adaptation helper used with the current `MemoryManager` port.
/// Upstream `GenericEnvironment` stores a `Tegra::MemoryManager*` and lets
/// it fetch shader bytes directly. The Rust `MemoryManager` still requires a
/// CPU-address reader callback to complete `read_block(...)`, so this callback
/// is kept as the closest available transport until that owner graph is made
/// literal too.
pub type GpuMemoryReader = Arc<dyn Fn(GPUVAddr, &mut [u8]) + Send + Sync>;
```

---

## 12. Logging

| Upstream                  | Rust                       |
| --------------------------| ---------------------------|
| `LOG_TRACE(Class, ...)`   | `log::trace!(...)`         |
| `LOG_DEBUG(Class, ...)`   | `log::debug!(...)`         |
| `LOG_INFO(Class, ...)`    | `log::info!(...)`          |
| `LOG_WARNING(Class, ...)` | `log::warn!(...)`          |
| `LOG_ERROR(Class, ...)`   | `log::error!(...)`         |
| `LOG_CRITICAL(Class, ...)`| `log::error!(...)`         |
| `UNIMPLEMENTED_MSG(...)`  | `log::error!("UNIMPLEMENTED: ..."); panic!(...)` only when upstream actually aborts |

Drop the upstream class tag (it is implicit from the module path) and
keep the format string identical apart from `{:08x}` → `{:#010X}` style
preferences already established in the file.

---

## 13. Diagnostic instrumentation

Investigation hooks live behind environment variables prefixed
`RUZU_TRACE_*`, `RUZU_DUMP_*`, `RUZU_FIND_*`, `RUZU_WATCH_*`, etc. They:

- Read once with `OnceLock`.
- Use `AtomicU64` / `AtomicBool` for one-shot or rate-limited firing.
- Print to `eprintln!` with a tagged prefix (`[INSTANCE]`, `[WORD_HIT]`,
  `[KCV_WAKE]`, etc.) so output is greppable.
- Have **zero** runtime cost when the env var is unset — a single env
  lookup followed by an early return.

Example:

```rust
fn should_trace_kcv_wake() -> bool {
    std::env::var_os("RUZU_TRACE_KCV_WAKE").is_some()
}

if should_trace_kcv_wake() {
    log::info!(
        "KCV_WAKE signal_impl waiter_tid={} addr=0x{:X} prev_tag=0x{:08X}",
        waiting_thread_id,
        address.get(),
        prev_tag,
    );
}
```

Multi-argument env hooks parse with `split(':')` and accept hex or decimal
on each segment:

```rust
let raw = std::env::var("RUZU_FIND_WORD_AT_SIGNAL").ok()?;
let parts: Vec<&str> = raw.split(':').collect();
if parts.len() != 3 { return None; }
let value = u32::from_str_radix(parts[0].trim_start_matches("0x"), 16).ok()?;
```

These hooks are fair game to land on `main` provided they default to off.
List them in a comment near the function so someone debugging later finds
them.

---

## 14. Tests

Tests live at the bottom of the file they test, in a `mod tests` block:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_method_blit_trigger_sets_pending() {
        let mut eng = Fermi2D::default();
        eng.call_method(BLIT_TRIGGER, 1, true);
        assert!(eng.pending_blit);
    }

    #[test]
    fn test_call_multi_method_propagates_upstream_last_call_flag() {
        let mut eng = Fermi2D::default();
        eng.call_method_last_flags.clear();
        eng.call_multi_method(0x100, &[1, 2, 3], 3, 3);
        assert_eq!(eng.call_method_last_flags, vec![false, false, true]);
    }
}
```

Rules:

- One test per parity-sensitive behavior (last-call propagation, payload
  layout, deep-dependency collapse, etc.). Don't write broad
  "does-not-crash" tests.
- Test names start with `test_` and describe the **expectation**, not the
  function name. `test_call_method_rejects_registers_past_upstream_num_regs`
  is good; `test_call_method_works` is bad.
- For test-only fields (e.g. `call_method_last_flags`), gate with
  `#[cfg(test)]` on the field declaration too.
- When testing against an upstream oracle (a32_diff, MT19937 reference),
  hard-code the expected output values and reference the seed/source in a
  comment.

---

## 15. DIFF.md entries

Every owner touched in a pass gets one entry in `DIFF.md`. The block is
mechanical:

```markdown
## YYYY-MM-DD — `<rust_path>` vs `<upstream_h_path>` and `<upstream_cpp_path>` (<scope>)

### Intentional differences
- <bullet>

### Unintentional differences (to fix)
- Fixed in this pass: <bullet describing the parity gap that was just closed>
- <still-open bullet>

### Missing items
- <upstream feature/method not yet ported>

### Binary layout verification
- PASS: <one-line>  (or  FAIL: <one-line>)
```

Conventions:

- The header date is the *day the entry was written*, not the upstream
  commit date.
- `<scope>` in parentheses tags the slice — e.g. `(CallMethod owner follow-up)`,
  `(surface-copy signature follow-up)`, `(GraphicsPipelineKey bitfield helper follow-up)` —
  so multiple entries on the same file in one pass remain navigable.
- "Intentional differences" are **deliberate Rust adaptations** the
  reviewer should accept (e.g. `Arc<Mutex<...>>` instead of raw pointer).
- "Unintentional differences (to fix)" track parity bugs. **"Fixed in
  this pass:"** prefix every bullet that the current diff resolves; bullets
  without that prefix are still open.
- "Missing items" enumerates upstream behavior not yet ported.
- "Binary layout verification" is required for any owner that touches
  `#[repr(C)]` types or guest-serialized payloads. Otherwise PASS
  with `no raw serialized struct involved.`

Append new entries at the **bottom** of `DIFF.md`. Do not edit existing
entries; instead add a new dated entry for the same path with a
`(<topic> follow-up)` scope tag.

---

## 16. Commit messages

A pass that touches multiple files gets one commit. Title lists the
biggest owner(s) and the high-level intent:

```
sync-cell branch: shader_environment / Fermi2D / Maxwell3D parity + kernel sync fixes
```

Body is grouped by subsystem with bulleted slice-level notes. Each bullet
names the upstream method or behavior that moved, never the line count or
the diff size. Example:

```
video_core engines:
- fermi_2d: restore CallMethod/CallMultiMethod last-call dispatch and surface-copy
  owner; fermi_2d now owns its memory manager (channel_state passes it in).
- maxwell_3d: shader-environment bridge, draw-manager topology bridge, OpenGL
  shader-key owner accessors.

Kernel:
- k_condition_variable: route update_lock_atomic through the process-owned
  ExclusiveMonitor (CAS loop) when initialized, falling back to the serialized
  RMW for bare-process tests.
- k_hardware_timer: take thread_ptrs out of the state lock during delivery so
  per-task callbacks can re-enter the timer without self-deadlocking.
```

Always end with the DIFF.md note (`DIFF.md: parity-audit entries for every
owner touched in this pass.`) and the standard `Co-Authored-By` trailer.

---

## 17. Workflow per owner

For each upstream `.h`/`.cpp` pair you touch:

1. **Read upstream first.** Open `<zuyu>/src/<path>.cpp` and `.h`. List
   every method, constant, member, helper, validation rule.
2. **Identify the Rust counterpart.** Path mirrors upstream
   (`zuyu/src/video_core/foo.cpp` → `ruzu/video_core/src/foo.rs`).
   If the Rust file does not exist yet, create it with the file header
   from §1 and stop to confirm the placement.
3. **Port names, signatures, and ordering verbatim** (snake_case only).
   Re-export shared types instead of duplicating them (§4).
4. **Adapt only where Rust forces it** — `Arc<Mutex<...>>`, trait objects,
   `Result`, `Drop`. Document each adaptation in the owner file *and* in
   the DIFF.md entry as an "Intentional difference" (§15).
5. **Add a focused regression test** (§14) for any non-obvious behavior
   you touched (last-call propagation, padding zero-init, hash output,
   exclusive monitor edge case, etc.).
6. **Write the DIFF.md entry** before you commit. If the bullet under
   "Unintentional differences (to fix)" doesn't start with "Fixed in this
   pass:", leave it open for the next pass.
7. **Run the smallest useful test first** (`cargo test -p <crate>
   <module>::tests::test_<name>`), then `cargo check --workspace` before
   committing.

If at any step you find yourself writing "TODO", `todo!()`, a stub
function that returns a dummy value, or a comment that says "implement
later", **stop**. The contract in `CLAUDE.md` § 3 forbids stubs. Either
implement the real behavior, or raise the dependency depth as a blocker.

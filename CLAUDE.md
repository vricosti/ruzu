# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ruzu is a Nintendo Switch emulator written in Rust, ported from yuzu. It uses rdynarmic (a Rust port of dynarmic) for ARM64 JIT recompilation, HLE (high-level emulation) of the HorizonOS kernel, and a software GPU rasterizer with Vulkan presentation.

### Reference Projects

- **zuyu** (https://github.com/vricosti/zuyu) — Fork of yuzu used as the C++ reference implementation for porting.
- **rxbyak** (https://github.com/vricosti/rxbyak) — Rust port of xbyak (x86-64 runtime assembler), used by rdynarmic for JIT code emission.
- **rdynarmic** (https://github.com/vricosti/rdynarmic) — Rust port of dynarmic (ARM64 dynamic recompiler). x64 backend is feature-complete: all ~650 IR opcodes wired, ~130 native SSE vector ops, ~220 stack-fallback vector ops. See rdynarmic/CLAUDE.md for details.

## Toolchain

```bash
cargo build --workspace
cargo test --workspace
cargo check --workspace
cargo clippy --workspace
```

Run a single test:
```bash
cargo test --workspace -- test_name
```

Run tests for a specific crate:
```bash
cargo test -p ruzu-loader
```

## Git

Never add `Co-Authored-By` or any reference to Claude in commit messages.

## Architecture

### Workspace Crates (dependency order)

```
ruzu-common    → Shared types: VAddr, Handle, ResultCode, PAGE_SIZE, base addresses
ruzu-crypto    → Key loading (prod.keys/title.keys), AES-CTR, AES-XTS decryption
ruzu-loader    → Game format parsing: NRO, NSP, XCI, NCA, NSO, PFS, RomFS
ruzu-cpu       → ARM64 interpreter: decoder, pattern_decoder, interpreter/{alu,mem,branch,simd,neon,crypto,system}
ruzu-kernel    → HorizonOS HLE kernel: process, threads, memory manager, SVC dispatch, handle table, scheduler
ruzu-gpu       → GPU emulation: GPFIFO command processor, Maxwell 3D/Kepler/Fermi engines, software rasterizer, macro interpreter
ruzu-service   → 40+ IPC services: sm, hid, vi, nvdrv, am, fsp-srv, audio, account, time, etc.
ruzu           → Main binary: boot sequence, SDL2 window, Vulkan presenter, main CPU loop
```

### Execution Flow

1. **Boot:** Parse CLI args → load keys → detect format (NRO/NSP/XCI) → load game
2. **Kernel init:** Create `KernelCore` → create `KProcess` → map code segments → create main thread
3. **Service init:** Register 40+ services in `ServiceManager`, wire up `ServiceBridge` for IPC
4. **Main loop:** 10 CPU time slices per frame (~60 FPS). Each slice: run interpreter (50k instruction budget) → handle SVC/context switch → signal vsync/audio events → flush GPU → present

### IPC Path (most common extension point)

```
Game SVC 0x21 (SendSyncRequest) → kernel dispatch_svc()
  → ipc_handler.handle_ipc() → ServiceManager.handle_request()
  → ServiceHandler::handle_request(cmd_id, &IpcCommand) → IpcResponse
```

Services implement the `ServiceHandler` trait (`ruzu-service/src/framework.rs`). Sub-interfaces are created dynamically via `get_sub_interface()` routing in `ruzu/src/main.rs`.

IPC protocol: HIPC header in TLS word 0-1, CMIF payload with magic SFCI (request) / SFCO (response), then raw data words. Parsed in `ruzu-service/src/ipc.rs`.

### CPU Emulation

ARM64 JIT recompilation via rdynarmic. ARM64 instructions are translated to an SSA IR (~650 opcodes), optimized, then emitted as native x86-64 code using rxbyak. `A64JitState` holds x0-x30, sp, pc, NZCV flags, v0-v31 (128-bit SIMD), tpidr_el0. The JIT returns `HaltReason::Svc(n)` or `BudgetExhausted` to yield control.

A fallback interpreter is also available in ruzu-cpu. `CpuState` with two-stage decoder (pattern decoder fast path → full decoder fallback). Instruction execution split across: `alu.rs`, `mem.rs`, `branch.rs`, `simd.rs`, `neon.rs`, `crypto.rs`, `system.rs`.

### Kernel Memory Model

4 GiB sparse backing store (anonymous mmap). Flat page table array mapping guest VA → backing offset. BTreeMap of `MemoryRegion` for QueryMemory. 39-bit address space: code at 0x0800_0000, heap growing up from same base, stack at 0x07FF_0000 growing down.

### GPU Pipeline

Games submit GPFIFO entries via nvdrv ioctl → `CommandProcessor` dispatches to engines (Maxwell 3D, Kepler Compute, Fermi 2D, Maxwell DMA, Inline-to-Memory) → software `Rasterizer` produces framebuffer → presented via Vulkan or software blit.

### Adding a New Service

1. Create handler struct implementing `ServiceHandler` in `ruzu-service/src/`
2. Implement `service_name()` and `handle_request(cmd_id, command) -> IpcResponse`
3. Register in `ServiceManager` setup in `ruzu/src/main.rs`
4. For sub-interfaces, add routing entry in `get_sub_interface()`

# Ruzu Emulator — Architecture Document

## 1. Overview

Ruzu is a Rust port of the yuzu/zuyu Nintendo Switch emulator. It executes Switch ARM guest code on an x86-64 host via a JIT compiler (rdynarmic), emulating the Switch kernel's SVC interface and HLE (High-Level Emulation) services.

### Crate layout

```
ruzu/
├── yuzu_cmd/         CLI frontend — entry point, SVC dispatch loop
├── core/             Emulator core — kernel, services, loader, ARM interface
│   └── src/
│       ├── core.rs              System object, boot sequence
│       ├── arm/                 ARM interface + JIT backends
│       │   ├── arm_interface.rs     ArmInterface trait
│       │   ├── dynarmic/
│       │   │   ├── arm_dynarmic_32.rs   AArch32 JIT backend
│       │   │   └── arm_dynarmic_64.rs   AArch64 JIT backend
│       │   └── nce/                 Native Code Execution backend
│       ├── hle/
│       │   ├── kernel/              Kernel object emulation
│       │   │   ├── k_process.rs         KProcess + ProcessMemoryData
│       │   │   ├── k_thread.rs          KThread + ThreadContext
│       │   │   ├── k_page_table_base.rs KPageTableBase (memory regions)
│       │   │   ├── k_memory_block*.rs   Memory block manager
│       │   │   ├── physical_core.rs     PhysicalCore (run loop)
│       │   │   ├── svc_dispatch.rs      SVC dispatch (call32/call64)
│       │   │   └── svc/                 Individual SVC handlers
│       │   └── service/             HLE service implementations
│       │       ├── sm/                  Service Manager (sm:)
│       │       ├── am/                  Applet Manager (appletOE)
│       │       ├── filesystem/          Filesystem (fsp-srv)
│       │       ├── nvdrv/               GPU driver (nvdrv)
│       │       └── ...
│       ├── loader/              ROM loading (NSP, NCA, NSO)
│       └── crypto/              Key management, AES
├── common/           Shared utilities (fs, alignment, logging)
├── audio_core/       Audio subsystem
├── video_core/       GPU subsystem
├── shader_recompiler/ Shader translation
├── hid_core/         Input devices
└── ...
```

External companion projects:
- **rdynarmic** (`/home/vricosti/Dev/emulators/rdynarmic/`) — Rust port of Dynarmic ARM JIT compiler
- **rxbyak** (`/home/vricosti/Dev/emulators/rxbyak/`) — Rust port of Xbyak x86-64 assembler

---

## 2. Boot Sequence

```
main()                                    yuzu_cmd/src/main.rs
  ├─ System::new()                        core/src/core.rs
  ├─ system.initialize()                  Sets up kernel, services, timing
  ├─ system.load(filepath)                Loads ROM via AppLoader
  │    ├─ VFS: open NSP/NCA file
  │    ├─ Identify loader type
  │    ├─ AppLoaderDeconstructedRomDirectory::load()
  │    │    ├─ Read NPDM metadata (address space, title ID, priorities)
  │    │    ├─ Pass 1: compute module layout (tentative addresses)
  │    │    ├─ process.allocate_code_memory(code_base, code_size)
  │    │    ├─ process.initialize_thread_local_region_allocation()
  │    │    ├─ process.load_from_metadata()
  │    │    │    ├─ initialize_for_user() → initialize_for_process()
  │    │    │    │    └─ Sets up ASLR'd regions (alias, heap, stack, kernel_map)
  │    │    │    ├─ KCapabilities::initialize_for_user()
  │    │    │    └─ Generate random entropy
  │    │    └─ Pass 2: load NSO modules into memory
  │    │         └─ For each module: decompress, write to ProcessMemoryData,
  │    │            update block_manager (CODE/CODE_DATA state)
  │    └─ Return LoadParameters (priority, stack_size)
  │
  ├─ process.run(priority, stack_size, ...)
  │    ├─ Create main thread TLS (create_thread_local_region)
  │    ├─ Allocate stack (map_pages_at_address + update_region)
  │    ├─ Set max heap size
  │    ├─ Create main KThread (initialize_user_thread_with_tls)
  │    │    └─ reset_thread_context32: PC=entry, SP=stack_top, R0=0
  │    └─ Register thread in handle table
  │
  ├─ Create JIT (ArmDynarmic32::new)
  │    ├─ DynarmicCallbacks32 with SharedProcessMemory
  │    ├─ JitConfig (cycle counting, cache size, optimizations)
  │    └─ rdynarmic::A32Jit::new(config)
  │
  ├─ physical_core.initialize_guest_runtime()
  │    ├─ restore_thread_to_jit()
  │    │    ├─ jit.set_context(thread_context)
  │    │    └─ jit.set_tpidrro_el0(tls_address)    ← CP15 URO register
  │    └─ Store runtime state
  │
  └─ physical_core.run_loop()             ← MAIN EXECUTION LOOP
```

---

## 3. Memory Architecture

### 3.1 Guest Address Space (AArch32 example — MK8D)

```
0x00000000 ┌──────────────────────┐
           │  (unmapped)          │
0x00200000 ├──────────────────────┤ ← code_base (rtld)
           │  rtld                │  state=CODE, perm=RX
0x00204000 │  rtld .rodata        │  state=CODE, perm=R
0x00205000 │  rtld .data          │  state=CODE_DATA, perm=RW
0x00206000 ├──────────────────────┤
           │  main (text)         │  state=CODE, perm=RX
           │  main (rodata)       │  state=CODE, perm=R
           │  main (data)         │  state=CODE_DATA, perm=RW
0x01512000 ├──────────────────────┤
           │  subsdk0..subsdk4    │  (same pattern per module)
0x01723000 ├──────────────────────┤
           │  sdk (text+ro+data)  │
0x02391000 ├──────────────────────┤
           │  (gap — FREE)        │  state=FREE
0x02395000 ├──────────────────────┤
           │  TLS page (0x1000)   │  state=THREAD_LOCAL, perm=RW
0x02396000 ├──────────────────────┤
           │  Guard (4 pages)     │  state=FREE (GetNumGuardPages = 4)
0x0239A000 ├──────────────────────┤
           │  Stack (1 MiB)       │  state=STACK, perm=RW
0x0249A000 ├──────────────────────┤
           │  (FREE — available   │  state=FREE
           │   for heap growth)   │
0x40000000 ├──────────────────────┤ ← heap_region_start (ASLR'd)
           │  Heap (SetHeapSize)  │  state=NORMAL, perm=RW
           │  up to 0x78000000   │  (sparse pages — allocated on write)
           ├──────────────────────┤
           │  (FREE)              │
0xFFFFFFFF └──────────────────────┘ ← 4 GiB boundary (32-bit AS)
```

### 3.2 ProcessMemoryData

Defined in `core/src/hle/kernel/k_process.rs`:

```rust
pub struct ProcessMemoryData {
    pub data: Vec<u8>,                        // Contiguous backing store
    pub base: u64,                            // Guest base address of data[]
    pub block_manager: KMemoryBlockManager,   // Tracks state/permissions
    pub sparse_pages: BTreeMap<u64, Vec<u8>>, // Demand-paged backing (heap)
}
```

Shared across the emulator as `SharedProcessMemory = Arc<RwLock<ProcessMemoryData>>`.

**Contiguous data (`data: Vec<u8>`):**
- Covers `[base, base + data.len())` — code modules, TLS, stack
- Pre-allocated during `allocate_code_memory()` and grown during `run()` for stack
- Direct-indexed: `offset = vaddr - base`, then `data[offset]`
- Fast path for JIT reads/writes in the code+stack region

**Sparse pages (`sparse_pages: BTreeMap<u64, Vec<u8>>`):**
- Covers everything outside the contiguous range (primarily the heap)
- Each entry: `page_base → Vec<u8>` of `PAGE_SIZE` (4096) bytes
- **Read**: returns 0 if page doesn't exist (zero-initialized)
- **Write**: creates page on first write (demand allocation)
- Avoids allocating the full ~2 GiB heap in host memory

**Read/Write path (e.g. `read_8`):**
```
read_8(vaddr):
  offset = vaddr - self.base
  if offset < data.len():
    return data[offset]            ← fast path (contiguous)
  else:
    return read_sparse_8(vaddr)    ← slow path (sparse pages)
```

**Write protection:**
```
is_writable(vaddr):
  block = block_manager.find_block(vaddr)
  if block.state == FREE:
    return true                    ← allow setup writes
  return block.permission.contains(USER_WRITE)
```

The JIT's `DynarmicCallbacks32::memory_write_*` methods check `is_writable()` before every write. Blocked writes are silently dropped (with a trace log).

### 3.3 Two Block Managers

Ruzu currently has two separate `KMemoryBlockManager` instances:

| | ProcessMemoryData::block_manager | KPageTableBase::m_memory_block_manager |
|---|---|---|
| **Location** | `k_process.rs` (field of ProcessMemoryData) | `k_page_table_base.rs` (field of KPageTableBase) |
| **Used by** | QueryMemory SVC, `is_writable()` | `set_heap_size()`, `can_contain()`, `map_pages_at_address()` |
| **Populated by** | NSO loader (`update_region`), `create_thread_local_region`, `run()` for stack, `set_heap_size` | `initialize_for_process()`, `map_pages_at_address()`, `set_heap_size()` |
| **Note** | This is the one guest code observes via QueryMemory | Internal kernel tracking; not directly visible to guest |

In upstream yuzu, there is a **single** `KMemoryBlockManager` inside `KPageTableBase`, and all operations (mapping, querying) go through it. The dual-manager design in ruzu is a porting artifact. Both managers must be kept in sync for any region that the guest might query.

### 3.4 Memory Regions (KPageTableBase)

Configured by `initialize_for_process()` with ASLR randomization:

```
m_address_space_start / m_address_space_end   // Full AS (0 — 0x100000000 for 32-bit)
m_code_region_start / m_code_region_end       // Code modules
m_alias_region_start / m_alias_region_end     // Alias mappings (IPC buffers)
m_heap_region_start / m_heap_region_end       // Heap (SetHeapSize grows here)
m_stack_region_start / m_stack_region_end     // Stack
m_kernel_map_region_start / m_kernel_map_region_end  // Kernel-mapped (static, TLS)
m_alias_code_region_start / m_alias_code_region_end  // Alias code region
```

`GetRegionAddress` and `GetRegionSize` map `Svc::MemoryState` values to these regions, matching upstream's switch statement exactly. Convenience overloads (`get_region_address_k`, etc.) convert from internal `KMemoryState` via `static_cast<Svc::MemoryState>(state & Mask)`, matching the inline overloads in upstream's `k_page_table_base.h`.

**Kernel vs user processes (`m_is_kernel`):**

`KPageTableBase` tracks whether the page table belongs to a kernel or user process via the `m_is_kernel` field (set during `initialize_for_process`). This affects memory layout behavior:

| | Kernel process | User process |
|---|---|---|
| `GetNumGuardPages()` | 1 (4 KiB) | 4 (16 KiB) |
| Guard pages purpose | Smaller guard between mappings | Larger guard for stack overflow detection |

Guard pages are FREE regions inserted by `MapPages` (find-free variant) before each mapping. They are not backed by physical memory — any access would fault. In the current port, `KProcess::run()` uses `get_num_guard_pages()` to place the stack with the correct gap after TLS, matching upstream's `MapPages` find-free behavior.

---

## 4. JIT Execution (rdynarmic)

### 4.1 Architecture

```
┌─────────────────────────────────────────────────┐
│  PhysicalCore::run_loop()                       │
│    ├─ jit.run()  ──────────────────────────────►│
│    │                   rdynarmic::A32Jit         │
│    │                   ┌─────────────────────┐   │
│    │                   │ Translate ARM → x86  │   │
│    │                   │ Execute x86 code     │   │
│    │                   │                      │   │
│    │                   │ Memory access ───────┼──►│ DynarmicCallbacks32
│    │                   │   read_code()        │   │   ├─ ProcessMemoryData::read_*
│    │                   │   read_8/16/32/64()  │   │   └─ ProcessMemoryData::write_*
│    │                   │   write_8/16/32/64() │   │       (with is_writable check)
│    │                   │                      │   │
│    │                   │ SVC instruction ─────┼──►│ call_supervisor(svc_num)
│    │                   │   → store svc_swi    │   │   → stores in Arc<AtomicU32>
│    │                   │   → halt with SVC    │   │
│    │                   └─────────────────────┘   │
│    │                                              │
│    ◄─ HaltReason::SUPERVISOR_CALL ───────────────│
│    │                                              │
│    ├─ jit.get_svc_number()                       │
│    ├─ jit.get_svc_arguments()  → args[0..7]     │
│    ├─ svc_dispatch::call(svc_num, args, ctx)     │
│    ├─ jit.set_svc_arguments(args)  ← results    │
│    └─ loop back to jit.run()                     │
└─────────────────────────────────────────────────┘
```

### 4.2 ArmDynarmic32

Defined in `core/src/arm/dynarmic/arm_dynarmic_32.rs`:

```rust
pub struct ArmDynarmic32 {
    jit: Option<rdynarmic::A32Jit>,     // The JIT instance
    svc_swi: Arc<AtomicU32>,            // SVC number (shared with callbacks)
    cp15_uro: u32,                      // TPIDRURO (TLS pointer for guest)
    core_index: usize,
    last_exception_address: Arc<AtomicU64>,
}
```

**Context transfer** between kernel and JIT:

| Direction | Method | What it transfers |
|---|---|---|
| Kernel → JIT | `set_context(ctx)` | R0-R15, CPSR, VFP regs, FPSCR, CP15 UPRW |
| Kernel → JIT | `set_tpidrro_el0(val)` | CP15 URO (TLS address) |
| JIT → Kernel | `get_context(ctx)` | Same fields in reverse |

**CP15 coprocessor registers (AArch32):**
- `CP15 C13,C0,2` (TPIDR_UPRW) — read-write thread pointer, stored in `ctx.tpidr`
- `CP15 C13,C0,3` (TPIDR_URO) — read-only thread pointer, stores TLS address
- When guest executes `MRC p15, 0, Rn, c13, c0, 3`, rdynarmic reads `cp15_uro` from JIT state

### 4.3 DynarmicCallbacks32

Implements `rdynarmic::JitCallbacks` trait:

```rust
struct DynarmicCallbacks32 {
    memory: SharedProcessMemory,        // Arc<RwLock<ProcessMemoryData>>
    svc_swi: Arc<AtomicU32>,           // SVC number output
    uses_wall_clock: bool,
    core_timing: Arc<Mutex<CoreTiming>>,
    last_exception_address: Arc<AtomicU64>,
}
```

Key callbacks:
- `memory_read_code(vaddr)` → reads instruction, returns UDF (0xE7F000F0) if unmapped
- `memory_read_*/write_*` → delegates to ProcessMemoryData with write protection
- `call_supervisor(svc_num)` → stores SVC number in `svc_swi`
- `add_ticks(ticks)` → advances CoreTiming (divided by NUM_CPU_CORES=4)
- `get_ticks_remaining()` → returns CoreTiming downcount

---

## 5. SVC Dispatch

### 5.1 Dispatch Flow

```
physical_core.rs: run_loop()
  │
  ├─ JIT halts with SUPERVISOR_CALL
  │
  ├─ jit.get_context(thread_context)     // save guest state
  ├─ on_supervisor_call(...)             // user callback (logging in yuzu_cmd)
  │
  ├─ dispatch_supervisor_call()
  │    ├─ svc_dispatch::call(svc_num, is_64bit, args, ctx)
  │    │    └─ call32(imm, args, ctx)    // match on SvcId enum
  │    │         ├─ SetHeapSize → svc_physical_memory
  │    │         ├─ QueryMemory → query_memory_info()
  │    │         ├─ SendSyncRequest → svc_ipc
  │    │         ├─ ConnectToNamedPort → svc_port
  │    │         └─ ...
  │    ├─ jit.set_svc_arguments(args)    // write back results
  │    └─ handoff_after_svc()            // thread scheduling
  │
  └─ loop
```

### 5.2 Argument Marshalling (AArch32)

SVC arguments are passed in registers R0-R7 (mapped to `args[0..7]`).

The AArch32 ABI for 64-bit values splits them across two 32-bit registers:

```rust
get_arg32(args, n) → args[n] as u32           // read r<n> as u32
set_arg32(args, n, val)                        // write r<n>
gather64(args, lo, hi) → lo32 | (hi32 << 32)  // read u64 from r<lo>:r<hi>
scatter64(args, lo, hi, val)                   // write u64 to r<lo>:r<hi>
```

The layout for each SVC follows the upstream `SvcWrap_*64From32` wrappers. Example:

```
SetHeapSize:
  IN:  size = get_arg32(args, 1)
  OUT: result = set_arg32(args, 0), heap_addr = set_arg32(args, 1)

QueryMemory:
  IN:  info_ptr = get_arg32(args, 0), query_addr = get_arg32(args, 2)
  OUT: result = set_arg32(args, 0), page_info = set_arg32(args, 1)
  SIDE EFFECT: writes MemoryInfo struct to info_ptr in guest memory

GetInfo:
  IN:  info_subtype = gather64(args, 0, 3), info_type = get_arg32(args, 1),
       handle = get_arg32(args, 2)
  OUT: result = set_arg32(args, 0), value = scatter64(args, 1, 2)
```

### 5.3 SvcContext

Passed to all SVC handlers:

```rust
pub struct SvcContext {
    pub shared_memory: SharedProcessMemory,
    pub code_base: u64,
    pub code_size: u64,
    pub stack_base: u64,
    pub stack_size: u64,
    pub program_id: u64,
    pub tls_base: u64,
    pub current_process: Arc<Mutex<KProcess>>,
    pub service_manager: Arc<Mutex<ServiceManager>>,
    pub scheduler: Arc<Mutex<KScheduler>>,
    pub next_thread_id: Arc<AtomicU64>,
    pub next_object_id: Arc<AtomicU32>,
    pub is_64bit: bool,
}
```

### 5.4 Key SVCs

| SVC | Number | Purpose |
|-----|--------|---------|
| SetHeapSize | 0x01 | Grow/shrink process heap |
| QueryMemory | 0x06 | Query memory state at address |
| ExitProcess | 0x07 | Terminate process |
| CreateThread | 0x08 | Create new thread |
| SetThreadPriority | 0x0D | Set thread priority |
| CreateTransferMemory | 0x15 | Create shared memory handle |
| CloseHandle | 0x16 | Release kernel object handle |
| WaitSynchronization | 0x18 | Wait on synchronization objects |
| SignalProcessWideKey | 0x1D | Condition variable signal |
| GetSystemTick | 0x1E | Read monotonic tick counter |
| ConnectToNamedPort | 0x1F | Connect to service port (e.g. "sm:") |
| SendSyncRequest | 0x21 | IPC request to service |
| GetThreadId | 0x25 | Get thread ID |
| Break | 0x26 | Userspace panic |
| GetInfo | 0x29 | Query process/system info |

---

## 6. IPC and HLE Services

### 6.1 IPC Flow

```
Guest calls SVC SendSyncRequest(handle)
  │
  ├─ Read IPC command from TLS buffer (guest addr = thread.tls_address)
  │    ├─ Parse CMIF header (cmd_type, cmd_id)
  │    └─ Extract data payload, buffer descriptors, copy/move handles
  │
  ├─ Lookup session object from handle table
  │    ├─ If domain session: route by domain object ID
  │    └─ If normal session: direct dispatch
  │
  ├─ Call HLE service handler method
  │    └─ Service writes response to TLS buffer
  │
  └─ Return result code in r0
```

### 6.2 Service Registration

Services are registered in `ServiceManager` during `System::initialize()`:

```
ServiceManager
  ├─ "sm:" → SM (Service Manager itself)
  ├─ "appletOE" → ApplicationProxyService (AM)
  ├─ "apm" → APM (Performance Manager)
  ├─ "lm" → LogManager
  ├─ "fsp-srv" → FileSystemProxy
  ├─ "nvdrv" → NvdrvInterface
  ├─ "pctl:a" → ParentalControl
  ├─ "aoc:u" → AddOnContent
  └─ ...
```

### 6.3 Domain Sessions

Some services use domain sessions (single connection, multiple sub-objects):

```
ConnectToNamedPort("sm:") → handle
SendSyncRequest(handle, Control::ConvertCurrentObjectToDomain)
SendSyncRequest(handle, Request::cmd=0)            → creates sub-object (domain_id=2)
SendSyncRequest(handle, Request::cmd=1, domain=2)  → calls sub-object method
```

Used by appletOE (AM service) which creates IApplicationProxy, ICommonStateGetter, ISelfController, IWindowController, etc. as domain sub-objects.

---

## 7. Kernel Objects

### 7.1 KProcess

```rust
pub struct KProcess {
    pub page_table: KProcessPageTable,
    pub process_memory: SharedProcessMemory,
    pub handle_table: KHandleTable,
    pub capabilities: KCapabilities,
    pub program_id: u64,
    pub code_address: KProcessAddress,
    pub code_size: usize,
    pub state: ProcessState,      // Created → Running → Terminated
    pub entropy: [u64; 4],        // Random entropy for ASLR
    pub max_process_memory: usize,
    pub main_thread_stack_size: usize,
    // ...
}
```

**Lifecycle:** `new()` → `load_from_metadata()` → `run()` → guest executes → `terminate()`

### 7.2 KThread

```rust
pub struct KThread {
    pub thread_context: ThreadContext,     // Saved ARM register state
    pub tls_address: KProcessAddress,      // Thread-local storage address
    pub priority: i32,                     // 0-63
    pub state: ThreadState,                // INITIALIZED | RUNNABLE | WAITING | TERMINATED
    pub thread_id: u64,
    // ...
}
```

**ThreadContext** holds all ARM registers (R0-R28, FP, LR, SP, PC, PSTATE, V0-V31, FPCR, FPSR, TPIDR).

### 7.3 Handle Table

Kernel handles encode `(index, linear_id)` into a u32:
```
handle = (linear_id << 15) | index
```

Maps handles → kernel objects (threads, sessions, events). Used by all SVCs that take handle arguments.

---

## 8. Loader

### 8.1 Loading Pipeline

```
NSP file
  └─ Extract NCA files
       ├─ Program NCA → ExeFS partition
       │    ├─ main.npdm        → ProgramMetadata (address space, title ID)
       │    ├─ rtld              → NSO (loaded at code_base)
       │    ├─ main              → NSO (loaded after rtld)
       │    ├─ subsdk0..subsdk4  → NSO (loaded sequentially)
       │    └─ sdk               → NSO (loaded last)
       └─ Control NCA → NACP + icons (metadata only)
```

### 8.2 Two-Pass Module Loading

**Pass 1 (layout):** Iterate all modules with `load_into_process=false`, compute tentative addresses and total `code_size`.

**Pass 2 (actual):** After process/page-table setup, iterate again with `load_into_process=true`:
- Decompress NSO sections (.text, .rodata, .data) via LZ4
- Write decompressed data to `ProcessMemoryData::data[]`
- Call `update_region()` to register each section in the block manager:
  - .text → state=CODE (0x03), perm=RX
  - .rodata → state=CODE (0x03), perm=R
  - .data → state=CODE_DATA (0x04), perm=RW

### 8.3 Static Module Order

```rust
const STATIC_MODULES: &[&str] = &[
    "rtld", "main",
    "subsdk0", "subsdk1", "subsdk2", "subsdk3", "subsdk4",
    "subsdk5", "subsdk6", "subsdk7", "subsdk8", "subsdk9",
    "sdk",
];
```

For MK8D (32-bit): rtld at 0x200000, main at 0x206000, subsdk0-4 and sdk follow contiguously.

---

## 9. Runtime Data Flow (MK8D boot example)

```
1. rtld (runtime linker) starts at PC=0x200000
     │
2. rtld calls QueryMemory in a loop to discover loaded modules
     │  → Walks address space, finds CODE/CODE_DATA/STACK/TLS/FREE blocks
     │  → Builds module list from memory map
     │
3. rtld resolves symbols (__nnDetailInitLibc0) across modules
     │
4. rtld connects to sm: via ConnectToNamedPort SVC
     │
5. rtld calls GetService for: lm, apm, appletOE, aoc:u, pctl:a
     │  → Each returns a session handle
     │  → appletOE converted to domain, sub-objects created
     │
6. Game code calls SetHeapSize(0x78000000)
     │  → Kernel allocates heap at heap_region_start (ASLR'd)
     │  → Returns heap base address
     │
7. rtld calls __nnDetailInitLibc0 for each module
     │  → Reads TPIDRURO (CP15 URO) for TLS base
     │  → Initializes SDK runtime: heap, TLS slots, C++ statics
     │
8. Game opens fsp-srv (filesystem) and nvdrv (GPU driver)
     │
9. Game enters main loop (rendering, input, audio)
```

---

## 10. Thread Lifecycle and Termination

The port uses `Arc<Mutex<KThread>>` and `Arc<Mutex<KProcess>>`, which constrains termination vs upstream where reentrance is more natural.

Three explicit "drain" boundaries handle termination checks:

1. **Scheduler boundary** (`k_scheduler.rs`) — after yield/wait
2. **SVC boundary** (`svc_dispatch.rs`) — after each SVC return
3. **CPU quantum boundary** (`physical_core.rs`) — after JIT halt

These cover the three families of kernel reentry: voluntary (SVC), cooperative (scheduler), implicit (quantum end). The worker task manager (`KWorkerTaskManager`) defers cleanup to avoid deadlocks under `MutexGuard`.

---

## 11. Known Porting Differences

| Area | Upstream (C++) | Ruzu (Rust) | Impact |
|------|---------------|-------------|--------|
| Memory block manager | Single, in KPageTableBase | Two separate (ProcessMemoryData + KPageTableBase) | Must keep in sync |
| Thread scheduling | Preemptive (timer IRQ) | Cooperative (SVC/quantum boundaries) | Drain points needed |
| ASLR randomization | KSystemControl::GenerateRandomRange | SystemTime-based hash | Non-deterministic but functional |
| KSynchronizationObject::Wait | Full implementation | Partial | Some waits are conservative |
| GPU/Display | Full Vulkan/OpenGL/Null | Stubs | Game crashes after nvdrv init |

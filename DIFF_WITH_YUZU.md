## 2026-03-21 — Windows-only code not ported

### Not ported (Windows platform)

The following upstream code is Linux-only in ruzu. The Windows code paths are not ported because ruzu targets Linux first:

- **`core/src/internal_network/network_interface.rs`** — `GetAdaptersAddresses` Windows API for enumerating network interfaces. Linux uses `getifaddrs` (fully ported).
- **`core/src/internal_network/sockets.rs`** — `WSASocket`/`WSAStartup`/`WSACleanup` Windows socket creation. Linux uses `libc::socket` (fully ported).
- **`core/src/tools/renderdoc.rs`** — `GetModuleHandleA`/`GetProcAddress` for loading RenderDoc on Windows. Linux uses `dlopen`/`dlsym` (fully ported).
- **`core/src/hle/service/ssl/ssl_backend_schannel.rs`** — Windows Schannel SSL backend. Linux uses OpenSSL (fully ported).

### Not ported (multiplayer network room)

- **`core/src/internal_network/socket_proxy.rs`** — `ProxySocket` that tunnels traffic through the multiplayer room network (`Network::RoomMember`). Requires the room networking system which is not yet ported. The `ProxySocket` struct exists but send/receive/decompress methods are stubs.

---

## 2026-03-19 — core/src/hle/service/hle_ipc.rs — ReadBuffer / WriteBuffer zero-copy divergence

### Intentional differences
- Upstream `HLERequestContext::ReadBuffer` returns `std::span<const u8>` — a zero-copy view into guest memory obtained via `memory.GetPointer(address)`. The span is valid for the lifetime of the SVC call (same scope as HLERequestContext).
- Ruzu's `HLERequestContext::read_buffer` returns `Vec<u8>` — a copy. This is necessary because the `Memory` bridge is behind `Arc<Mutex<Memory>>`, and returning a reference into locked memory would require holding the lock for the entire span lifetime, preventing any other Memory access during that time.
- Upstream has a separate `ReadBufferCopy` that returns `std::vector<u8>` for callers that explicitly want a copy. In ruzu, `read_buffer` always copies (matching `ReadBufferCopy` semantics), so there is no separate `read_buffer_copy`.

### Performance impact
- Negligible at this stage. IPC buffers are typically small (32-512 bytes for nvdrv IOCTLs, up to 64KB for filesystem reads). Cooperative single-threaded HLE means no lock contention. The memcpy cost is invisible compared to JIT reentry overhead around each SVC.
- If performance matters later, `Memory::get_pointer(address)` could return a raw `*const u8` into DeviceMemory and we could return an unsafe slice, but this is an optimization not a correctness issue.

---

## 2026-03-19 — core/src/hle/kernel/k_synchronization_object.rs vs zuyu/src/core/hle/kernel/k_synchronization_object.h/.cpp

### Intentional differences
- Upstream uses an intrusive linked list of `ThreadListNode` structs for the waiter list inside each `KSynchronizationObject`. Each `ThreadListNode { ThreadListNode* next; KThread* thread; }` is allocated on the stack inside `KSynchronizationObject::Wait()` and linked/unlinked via raw pointers — no locking required. Rust cannot safely use raw pointers in this pattern, so ruzu uses a `SynchronizationWaitSet` stored inside `KThread::synchronization_wait` (behind `Mutex<KThread>`) with handle-based indirection (`SynchronizationWaitNodeHandle { thread_id, wait_index }`).
- Because the wait node data lives behind `Mutex<KThread>`, helper functions `with_wait_object` / `with_wait_object_mut` must lock the waiting thread's mutex to access its `SynchronizationWaitSet`. This requires that `get_thread_by_thread_id` never iterate-and-lock all threads (which would deadlock when called while already holding a thread lock). Ruzu solves this with a reverse lookup map `thread_objects_by_thread_id: BTreeMap<u64, Arc<Mutex<KThread>>>` in `KProcess`, providing O(log n) lockless lookup. Upstream does not need this because `ThreadListNode` directly contains a `KThread*` pointer.
- Upstream's `LinkNode` / `UnlinkNode` are simple pointer manipulations on `KSynchronizationObject`'s `m_thread_list_head` / `m_thread_list_tail`. Ruzu's equivalent `link_waiter` / `unlink_waiter` go through `resolve_waitable_object` → lock the target object's mutex → delegate to `SynchronizationObjectState::link_waiter` → which calls `SynchronizationWaiters::link` → which calls `with_wait_object` to access the waiting thread's wait set. This multi-lock path is safe as long as the locking order is: target object first, then waiting thread (never the reverse).

### Unintentional differences (to fix)
- `clear_wait_set` and `consume_signaled_wait_set` have logic bugs where waiters are not correctly unlinked from target objects. The 2 failing tests (`clear_wait_set_unlinks_all_objects`, `consume_signaled_wait_set_unlinks_other_objects`) demonstrate this. Root cause: the `SynchronizationWaitSet` is moved into the thread by `begin_wait_synchronization`, so `unlink_wait_set` cannot find the wait objects via the handle's `thread_id` when the wait set has already been consumed. Needs investigation.

### Missing items
- None structurally.

### Binary layout verification
- N/A: not serialized.

---

## 2026-03-18 — core/src/hle/service/psc/time/static.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/static.cpp

### Intentional differences
- Rust module is exposed as `r#static` because `static` is a Rust keyword; the file path now matches upstream exactly.
- Sub-service creation methods (GetStandardUserSystemClock, etc.) create standalone service objects rather than referencing shared clock cores from TimeManager. Upstream passes clock core references from TimeManager; Rust creates new instances with the correct permission flags. Will converge once TimeManager is fully wired.
- CalculateMonotonicSystemClockBaseTimePoint uses `std::time::Instant` elapsed since boot instead of CoreTiming ticks. Behavioral parity is maintained (both measure time since process start in nanoseconds, converted to seconds).

### Unintentional differences (to fix)
- Upstream SetStandardUserSystemClockAutomaticCorrectionEnabled updates shared memory and signals the user clock event; Rust only updates local state.

### Missing items
- GetSharedMemoryNativeHandle (cmd 20): Now returns a real copy handle to the guest via IPC. The remaining gap is that `MapSharedMemory` SVC (0x13) is still a stub success — the guest gets the handle but the actual page mapping into guest address space is a no-op, so guest code cannot yet read the lock-free time data through mapped pages.

### Binary layout verification
- PASS: `ClockSnapshot`, `SteadyClockTimePoint`, `SystemClockContext`, and `StaticServiceSetupInfo` retain explicit size assertions in their owner modules.

## 2026-03-18 — core/src/hle/service/psc/time/manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/manager.h

### Intentional differences
- Rust TimeManager uses callback-based clock core construction rather than direct self-referential member references. Clock cores receive `Arc`-wrapped tick callbacks instead of references to sibling members.
- SharedMemory owns a `KSharedMemory` with `Vec<u8>` backing instead of pages from `DeviceMemory`.
- ContextWriters use `Vec<OperationEvent>` for the subscriber list instead of upstream's intrusive `IntrusiveListBaseTraits<OperationEvent>::ListType`. Multi-subscriber `Link`/`SignalAllNodes` behavior matches upstream. Shared memory access uses callbacks instead of direct references.
- `SystemClockCore` holds `Arc<Mutex<dyn ContextWriter>>` instead of a raw `ContextWriter*` pointer. `LinkOperationEvent` delegates to the writer's `link()` method, matching upstream.
- Alarms use a `Vec<AlarmEntry>` instead of upstream's intrusive linked list. Sorting behavior is identical (by alert_time then priority).
- Alarm::Lock() is a no-op stub, matching upstream where the lock service is also TODO.

### Unintentional differences (to fix)
- None currently.

### Missing items
- KSharedMemory uses `Vec<u8>` backing instead of DeviceMemory pages (will converge once KMemoryManager is wired)
- KEvent for PowerStateRequestManager, Alarms, Alarm, and OperationEvent signaling

### Binary layout verification
- PASS: `SharedMemoryStruct` has offset assertions matching upstream (0x0, 0x38, 0x80, 0xC8, 0xD0) and size assertion (0x1000).

## 2026-03-18 — core/src/hle/service/psc/time/time_zone_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/time_zone_service.cpp

### Intentional differences
- Rust TimeZoneService owns a local TimeZone instance instead of holding references to StandardSteadyClockCore and TimeZone from TimeManager. Will converge once TimeManager references are wired.

### Unintentional differences (to fix)
- None currently.

### Missing items
- SetDeviceLocationNameWithTimeZoneRule: needs steady clock time point update
- GetDeviceLocationNameOperationEventReadableHandle: needs KEvent

### Binary layout verification
- N/A: TimeZoneService is not serialized.

## 2026-03-18 — core/src/hle/service/psc/time/shared_memory.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/shared_memory.cpp

### Intentional differences
- Rust SharedMemory owns a `KSharedMemory` with `Vec<u8>` backing instead of pages allocated from `DeviceMemory` via `KMemoryManager`. The `SharedMemoryStruct` is accessed via raw pointer into this backing, matching upstream's `reinterpret_cast<SharedMemoryStruct*>(m_k_shared_memory.GetPointer())`.
- Lock-free read/write use `std::sync::atomic::fence` instead of `std::atomic_thread_fence`.

### Unintentional differences (to fix)
- None currently.

### Missing items
- None. `GetKSharedMemory()` is implemented.

### Binary layout verification
- PASS: `SharedMemoryStruct` size = 0x1000, field offsets match upstream (0x0, 0x38, 0x80, 0xC8, 0xD0).

## 2026-03-18 — core/src/hle/service/psc/time/power_state_request_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/power_state_request_manager.cpp

### Intentional differences
- None. All three methods match upstream behavior exactly.

### Unintentional differences (to fix)
- None currently.

### Missing items
- KEvent for Signal/Clear (currently no-ops)
- GetReadableEvent accessor

### Binary layout verification
- N/A: not serialized.

## 2026-03-18 — core/src/hle/service/psc/time/alarms.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/alarms.cpp

### Intentional differences
- Uses `Vec<AlarmEntry>` instead of upstream's `IntrusiveListBaseTraits<Alarm>::ListType`. Insertion sort behavior is identical.
- Alarm struct does not use intrusive list nodes; `linked` bool tracks registration state.
- IAlarmService and ISteadyClockAlarm IPC service structs are not yet ported (command IDs are defined).

### Unintentional differences (to fix)
- None currently.

### Missing items
- KEvent for Alarm signaling and Alarms closest-alarm-updated event
- ISteadyClockAlarm IPC service struct
- IAlarmService IPC service struct
- Alarm::Lock() with IPmStateLock

### Binary layout verification
- N/A: not serialized.

## 2026-03-18 — core/src/hle/service/glue/time/static.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/glue/time/static.cpp

### Intentional differences
- Rust module is exposed as `r#static` because `static` is a Rust keyword; the file path now matches upstream exactly.
- GetStandardUserSystemClockInitialYear returns hardcoded 2019 instead of delegating to set:sys. Will converge once ISystemSettingsServer is wired.
- SetStandardSteadyClockInternalOffset divides by 1e9 but doesn't delegate to set:sys SetExternalSteadyClockInternalOffset yet.

### Unintentional differences (to fix)
- Upstream resolves `set:sys`, `PSC::Time::ServiceManager`, wrapped `PSC::Time::StaticService`, `TimeZoneService`, `FileTimestampWorker`, `StandardSteadyClockResource`, and `TimeZoneBinary` inside this file; Rust still leaves the `set:sys` and `PSC::Time::ServiceManager` wiring as TODOs.
- Most "Get" methods (GetStandardUserSystemClock, etc.) that delegate to the wrapped PSC static service currently return placeholder success instead of creating proper sub-service IPC objects.

### Missing items
- set:sys integration for GetStandardUserSystemClockInitialYear and SetStandardSteadyClockInternalOffset
- Proper sub-service object creation and return via IPC for Get*Clock methods

### Binary layout verification
- PASS: no raw serialized structs are owned in this file.

## 2026-03-18 — core/src/hle/service/ssl/ssl_backend_openssl.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ssl/ssl_backend_openssl.cpp

### Intentional differences
- Uses the Rust `openssl` crate's `SslConnector`/`SslStream` instead of raw C OpenSSL API (`SSL_new`, `SSL_do_handshake`, `SSL_read_ex`, `SSL_write_ex`). Behavioral parity is maintained.
- Uses `SslStream<TcpStreamAdapter>` instead of custom BIO callbacks (`ReadCallback`, `WriteCallback`, `CtrlCallback`). Upstream needs custom BIO because it uses its own `Network::SocketBase`; Rust duplicates the socket FD into a `TcpStream` which provides standard `Read`/`Write` impls.
- `SslConnector::connect()` handles hostname verification (`SSL_set1_host`) and SNI (`SSL_set_tlsext_host_name`) in one call.
- SSLKEYLOGFILE support uses `SslContextBuilder::set_keylog_callback` instead of upstream's `SSL_CTX_set_keylog_callback` with a static `IOFile`.
- One-time initialization is simpler: the `openssl` crate handles `SSL_library_init` / `SSL_load_error_strings` automatically.

### Unintentional differences (to fix)
- `DoHandshake` WouldBlock path does not preserve the mid-handshake state for retry. Upstream can retry because it keeps the `SSL*`; we lose the `MidHandshakeSslStream`. This needs fixing for non-blocking sockets.

### Missing items
- Mid-handshake retry support for non-blocking `DoHandshake`
- `CheckOpenSSLErrors` equivalent for detailed error logging (upstream iterates `ERR_get_error_all`)

### Binary layout verification
- PASS: no raw serialized structs are owned in this file.

## 2026-03-18 — core/src/hle/service/ssl/ssl_backend_schannel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ssl/ssl_backend_schannel.cpp

### Intentional differences
- Rust keeps the file boundary for Schannel ownership but does not attempt Windows-only API integration yet.

### Unintentional differences (to fix)
- Upstream implements the full `SSLConnectionBackendSchannel` type and handshake state machine; Rust currently returns `RESULT_INTERNAL_ERROR`.

### Missing items
- Schannel backend type
- One-time credential initialization
- Handshake state machine
- Read / write buffering
- Server certificate extraction

### Binary layout verification
- PASS: no raw serialized structs are owned in this file.

## 2026-03-18 — core/src/hle/service/ssl/ssl_backend_securetransport.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ssl/ssl_backend_securetransport.cpp

### Intentional differences
- Rust keeps the file boundary for SecureTransport ownership but does not attempt macOS-only API integration yet.

### Unintentional differences (to fix)
- Upstream implements the full `SSLConnectionBackendSecureTransport` type, callback plumbing, and certificate export; Rust currently returns `RESULT_INTERNAL_ERROR`.

### Missing items
- SecureTransport backend type
- Handshake / read / write logic
- Socket callback plumbing
- Server certificate extraction

### Binary layout verification
- PASS: no raw serialized structs are owned in this file.


---

## 2026-03-27 — core/src/hle/kernel/k_priority_queue.rs, k_scheduler.rs, global_scheduler_context.rs — scheduler architecture divergence

### Intentional differences

**QueueEntry storage**: Upstream uses intrusive linked lists with `QueueEntry` nodes embedded directly in `KThread` (via `m_per_core_priority_queue_entry[4]`), accessed through raw `KThread*` pointers. Ruzu stores entries in a `HashMap<u64, [QueueEntry; 4]>` inside `KPriorityQueue` itself. This eliminates the `ThreadAccessor` / `KPriorityQueueMember` traits and allows PQ operations without locking any `KThread` mutex.

**Property cache**: Upstream reads thread properties (`priority`, `active_core`, `affinity_mask`, `is_dummy`) directly from `KThread*` pointers during PQ operations and scheduler migration. Ruzu caches these properties in `ThreadProps` inside the PQ, updated on push/remove/change operations. The migration loop reads from this cache instead of locking threads.

**Lock ordering (thread-process-GSC)**: Upstream uses a single `KAbstractSchedulerLock` (recursive spinlock) to serialize all scheduler operations -- there is no per-thread mutex. Ruzu wraps threads in `Arc<Mutex<KThread>>`, creating a lock ordering constraint: thread then process then GSC. The reverse direction (GSC then thread) is avoided to prevent deadlock. This is the root cause of the next difference.

**PQ updates via direct GSC reference on KThread**: Upstream's `KThread::SetState()` acquires `KScopedSchedulerLock` and calls `KScheduler::OnThreadStateChanged(kernel, this, old_state)` which immediately updates the PQ. Ruzu's `set_state()` calls `notify_state_transition()` which accesses the GSC directly via a `Weak<Mutex<GlobalSchedulerContext>>` stored on KThread (matching upstream's access via `KernelCore&`). The PQ push/remove happens inside `notify_state_transition` without going through the process lock. Some legacy call sites still do manual PQ push/remove (condvar, sync objects, hardware timer) — these should be audited for double-push risk.

**`IncrementScheduledCount` via `Arc<AtomicI64>`**: Upstream calls `thread->GetOwnerProcess()->IncrementScheduledCount()` by navigating from thread to process via a raw pointer. This is impossible in ruzu from within the GSC lock (would require thread then process locking, creating a deadlock). Instead, `KProcess::schedule_count` is an `Arc<AtomicI64>` shared between the process, the KThread, and the PQ's `ThreadProps` cache. `pq.increment_scheduled_count(thread_id)` does a lock-free `fetch_add(1, Relaxed)` on the shared atomic.

**Migration `core_id` update deferred**: Upstream calls `thread->SetActiveCore(new_core)` during migration inside `UpdateHighestPriorityThreadsImpl` under the scheduler lock. Ruzu cannot lock threads while holding the GSC lock (deadlock risk). Instead, `change_core` updates the PQ's cached `active_core`, and the actual `KThread::core_id` update is collected in a `Vec<(thread_id, new_core)>` and applied after releasing the GSC lock.

### Unintentional differences (to fix)

- Some call sites (condvar, sync objects, hardware timer) still do manual `push_back_to_priority_queue` / `remove_from_priority_queue` after state transitions that also go through `set_state()` → `notify_state_transition()`. These may cause benign double-push/double-remove but should be audited and removed where redundant.

### Missing items

- `IncrementScheduledCount` in `yield_with_core_migration` and `yield_to_any_thread` -- these currently delegate to `yield_without_core_migration` which has a single call. Upstream has 2 calls in each. Will be added when these yield variants are fully implemented.

### Binary layout verification

- N/A: scheduler state is not serialized.

---

## 2026-03-27 — common/src/fiber.rs — fiber entry point return handling

### Intentional differences

**Service thread fiber lifecycle**: Upstream wraps service thread entry points with `OnThreadStart()` → `func()` → `ExitThread()`. ExitThread calls `KThread::Exit()` which transitions to TERMINATED under `KScopedSchedulerLock`, ensuring the scheduler removes the thread and switches to the next. In ruzu, `coroutine_start` yields back to the switch fiber (via `yielder.suspend()`) when the entry function returns, instead of hitting `unreachable!()`. The service thread wrapper in `run_on_guest_core_process` transitions the thread to TERMINATED before the fiber function returns, so the PQ is updated before the switch fiber re-enters its scheduling loop.

**Switch fiber re-entry**: Upstream's switch fiber loop (`ScheduleImplFiber`) relies on `KScopedSchedulerLock` destructor calling `UpdateHighestPriorityThreads` before each scheduling decision. Ruzu's `schedule_impl_fiber_loop` explicitly calls `UpdateHighestPriorityThreads` via the scheduler lock's callback on each re-entry, to handle the case where a service thread's TERMINATED transition removed it from PQ but `highest_priority_thread_id` was stale.

### Unintentional differences (to fix)

- None currently.

### Binary layout verification

- N/A: fiber state is not serialized.

---

## Misc
//Translate outgoing copy objects handles.
//Upstream: handle_table.Add() for proper KAutoObject translation.
//For now, skip past — handles were written by push_move_objects/push_copy_objects.
//why is it different ? Would it be possible to be closer to upstream ?  

  Upstream: PushCopyObjects pushes a KAutoObject* into outgoing_copy_objects. Then WriteToOutgoingCommandBuffer calls handle_table.Add(object) to get a handle and
  writes it to cmd_buf.

  Ours: push_copy_objects takes a u32 handle directly, writes it to cmd_buf immediately, AND pushes it to outgoing_copy_objects. Then WriteToOutgoingCommandBuffer just
  skips past.

  The difference: upstream defers handle creation to WriteToOutgoingCommandBuffer, we do it eagerly in push_copy_objects. This is because upstream services create
  kernel objects (like KEvent) and push the object reference, while our services already have handle numbers.

  To match upstream exactly, we'd need:
  1. Change outgoing_copy_objects: Vec<u32> to Vec<Arc<dyn Any>> (kernel objects)
  2. push_copy_objects stores the kernel object, not a handle
  3. WriteToOutgoingCommandBuffer calls handle_table.Add() to create the handle

  But this requires the handle table to accept arbitrary kernel objects. Our KHandleTable stores u64 opaque handles, not KAutoObject trait objects.

  The behavioral result is identical — the guest receives the same handle value in the same IPC buffer position. The difference is architectural: we resolve handles
  early, upstream resolves them late. Both produce the same guest-visible result.

  Making it closer to upstream would require:
  1. A KAutoObject trait with Open()/Close() methods
  2. KHandleTable::Add(&dyn KAutoObject) -> Handle
  3. All services push trait objects instead of pre-resolved handles

  ----------------
  No changes needed in ruzu — the default false from the trait is used, matching upstream exactly.                                                                                      
                                                                                                                                                          
  The is_read_only_memory callback is now properly implemented:                                                                                                                         
                                                                                                                                                          
  1. UserCallbacks trait in rdynarmic has is_read_only_memory(&self, vaddr: u32) -> bool with default false                                                                             
  2. A32ConstantMemoryReads pass calls it via the is_read_only closure parameter
  3. get_or_compile_block_with_ro() threads the callback from run()/step()/trampoline                                                                                                   
  4. ruzu's DynarmicCallbacks32 inherits the default false, matching upstream's behavior                                                                                                
                                                                                                                                                                                        
  The callback is ready for a future enhancement where ruzu implements it using the process page table to identify read-only code sections (e.g., USER_READ_EXECUTE pages). That would  
  enable constant folding for literal pool loads — an optimization upstream doesn't even use.
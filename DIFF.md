## 2026-03-18 — core/src/hle/service/psc/time/static.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/static.cpp

### Intentional differences
- Rust module is exposed as `r#static` because `static` is a Rust keyword; the file path now matches upstream exactly.
- Sub-service creation methods (GetStandardUserSystemClock, etc.) create standalone service objects rather than referencing shared clock cores from TimeManager. Upstream passes clock core references from TimeManager; Rust creates new instances with the correct permission flags. Will converge once TimeManager is fully wired.
- GetClockSnapshotImpl uses a local UTC calendar conversion helper instead of delegating to TimeZone::ToCalendarTimeWithMyRule. Will converge once TimeZone reference is held.
- CalculateMonotonicSystemClockBaseTimePoint uses steady_clock_time_point approximation instead of CoreTiming ticks. Will converge once CoreTiming is wired.

### Unintentional differences (to fix)
- Upstream SetStandardUserSystemClockAutomaticCorrectionEnabled updates shared memory and signals the user clock event; Rust only updates local state.

### Missing items
- Shared memory integration (GetSharedMemoryNativeHandle requires KSharedMemory)
- CoreTiming integration for CalculateMonotonicSystemClockBaseTimePoint
- TimeZone reference for GetClockSnapshotImpl calendar conversion

### Binary layout verification
- PASS: `ClockSnapshot`, `SteadyClockTimePoint`, `SystemClockContext`, and `StaticServiceSetupInfo` retain explicit size assertions in their owner modules.

## 2026-03-18 — core/src/hle/service/psc/time/manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/manager.h

### Intentional differences
- Rust TimeManager uses callback-based clock core construction rather than direct self-referential member references. Clock cores receive `Arc`-wrapped tick callbacks instead of references to sibling members.
- SharedMemory owns a `Box<SharedMemoryStruct>` instead of referencing a kernel `KSharedMemory`. Will map to kernel shared memory once KSharedMemory is wired.
- ContextWriters use `Vec<OperationEvent>` for the subscriber list instead of upstream's intrusive `IntrusiveListBaseTraits<OperationEvent>::ListType`. Multi-subscriber `Link`/`SignalAllNodes` behavior matches upstream. Shared memory access uses callbacks instead of direct references.
- `SystemClockCore` holds `Arc<Mutex<dyn ContextWriter>>` instead of a raw `ContextWriter*` pointer. `LinkOperationEvent` delegates to the writer's `link()` method, matching upstream.
- Alarms use a `Vec<AlarmEntry>` instead of upstream's intrusive linked list. Sorting behavior is identical (by alert_time then priority).
- Alarm::Lock() is a no-op stub, matching upstream where the lock service is also TODO.

### Unintentional differences (to fix)
- None currently.

### Missing items
- KSharedMemory mapping for SharedMemory (uses Box instead)
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
- Rust SharedMemory owns a `Box<SharedMemoryStruct>` instead of a pointer into kernel `KSharedMemory`. Will switch to mapped kernel memory once KSharedMemory is available.
- Lock-free read/write use `std::sync::atomic::fence` instead of `std::atomic_thread_fence`.

### Unintentional differences (to fix)
- None currently.

### Missing items
- KSharedMemory integration (GetKSharedMemory)

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

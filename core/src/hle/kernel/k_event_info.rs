//! Port of zuyu/src/core/hle/kernel/k_event_info.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KEventInfo: debug event information structure used by KDebug.

/// Reasons a process can exit.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProcessExitReason {
    #[default]
    ExitProcess = 0,
    TerminateProcess = 1,
    Exception = 2,
}

/// Reasons a thread can exit.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ThreadExitReason {
    #[default]
    ExitThread = 0,
    TerminateThread = 1,
    ExitProcess = 2,
    TerminateProcess = 3,
}

/// Debug exception types.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DebugException {
    #[default]
    UndefinedInstruction = 0,
    InstructionAbort = 1,
    DataAbort = 2,
    AlignmentFault = 3,
    DebuggerAttached = 4,
    BreakPoint = 5,
    UserBreak = 6,
    DebuggerBreak = 7,
    UndefinedSystemCall = 8,
    MemorySystemError = 9,
}

/// Debug event types.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DebugEvent {
    #[default]
    CreateProcess = 0,
    CreateThread = 1,
    ExitProcess = 2,
    ExitThread = 3,
    Exception = 4,
}

/// Information for a thread creation debug event.
#[derive(Debug, Clone, Default)]
pub struct InfoCreateThread {
    pub thread_id: u32,
    pub tls_address: usize,
}

/// Information for a process exit debug event.
#[derive(Debug, Clone, Default)]
pub struct InfoExitProcess {
    pub reason: ProcessExitReason,
}

/// Information for a thread exit debug event.
#[derive(Debug, Clone, Default)]
pub struct InfoExitThread {
    pub reason: ThreadExitReason,
}

/// Information for an exception debug event.
#[derive(Debug, Clone, Default)]
pub struct InfoException {
    pub exception_type: DebugException,
    pub exception_data_count: i32,
    pub exception_address: usize,
    pub exception_data: [usize; 4],
}

/// Information for a system call debug event.
#[derive(Debug, Clone, Default)]
pub struct InfoSystemCall {
    pub tick: i64,
    pub id: i32,
}

/// Union-like enum for debug event info.
#[derive(Debug, Clone)]
pub enum EventInfoData {
    CreateThread(InfoCreateThread),
    ExitProcess(InfoExitProcess),
    ExitThread(InfoExitThread),
    Exception(InfoException),
    SystemCall(InfoSystemCall),
    None,
}

impl Default for EventInfoData {
    fn default() -> Self {
        EventInfoData::None
    }
}

/// Debug event information, used by KDebug to track debug events.
///
/// Upstream this is slab-allocated and an intrusive list node. The intrusive
/// list integration is TODO.
#[derive(Debug, Clone, Default)]
pub struct KEventInfo {
    pub event: DebugEvent,
    pub thread_id: u32,
    pub flags: u32,
    pub is_attached: bool,
    pub continue_flag: bool,
    pub ignore_continue: bool,
    pub close_once: bool,
    pub info: EventInfoData,
    // debug_thread: placeholder for *mut KThread
}

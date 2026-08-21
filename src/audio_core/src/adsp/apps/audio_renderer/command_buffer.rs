use crate::common::common::CpuAddr;

// Stub: KProcess lived in ruzu_kernel which has been removed.
// ProcessHandle stores the raw pointer as an opaque usize.
// When ruzu_kernel is restored, replace `*mut ()` with `*mut KProcess`.

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ProcessHandle(usize);

impl ProcessHandle {
    pub fn from_ptr(process: *mut ()) -> Self {
        Self(process as usize)
    }

    pub fn as_ptr(self) -> *mut () {
        self.0 as *mut ()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CommandBuffer {
    pub buffer: CpuAddr,
    pub size: u64,
    pub time_limit: u64,
    pub applet_resource_user_id: u64,
    pub process: ProcessHandle,
    pub reset_buffer: bool,
    pub remaining_command_count: u32,
    pub render_time_taken_us: u64,
}

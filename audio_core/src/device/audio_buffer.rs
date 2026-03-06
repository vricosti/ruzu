#[derive(Debug, Clone, Copy, Default)]
pub struct AudioBuffer {
    pub start_timestamp: u64,
    pub end_timestamp: u64,
    pub played_timestamp: i64,
    pub samples: u64,
    pub tag: u64,
    pub size: u64,
}

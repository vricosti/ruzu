//! Port of zuyu/src/core/hle/service/filesystem/fsp/save_data_transfer_prohibiter.h and .cpp
//!
//! ISaveDataTransferProhibiter service.

/// ISaveDataTransferProhibiter has no IPC commands.
/// It exists solely as an object reference.
pub struct ISaveDataTransferProhibiter;

impl ISaveDataTransferProhibiter {
    pub fn new() -> Self {
        Self
    }
}

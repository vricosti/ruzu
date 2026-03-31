//! Port of zuyu/src/core/hle/kernel/svc_results.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! Confirmed Switch kernel error codes used by SVC handlers.

use crate::hle::result::{ErrorModule, ResultCode};

// Confirmed Switch kernel error codes
pub const RESULT_OUT_OF_SESSIONS: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 7);
pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 14);
pub const RESULT_NOT_IMPLEMENTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 33);
pub const RESULT_NO_SYNCHRONIZATION_OBJECT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 57);
pub const RESULT_TERMINATION_REQUESTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 59);
pub const RESULT_INVALID_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 101);
pub const RESULT_INVALID_ADDRESS: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 102);
pub const RESULT_OUT_OF_RESOURCE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 103);
pub const RESULT_OUT_OF_MEMORY: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 104);
pub const RESULT_OUT_OF_HANDLES: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 105);
pub const RESULT_INVALID_CURRENT_MEMORY: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 106);
pub const RESULT_INVALID_NEW_MEMORY_PERMISSION: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 108);
pub const RESULT_INVALID_MEMORY_REGION: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 110);
pub const RESULT_INVALID_PRIORITY: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 112);
pub const RESULT_INVALID_CORE_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 113);
pub const RESULT_INVALID_HANDLE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 114);
pub const RESULT_INVALID_POINTER: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 115);
pub const RESULT_INVALID_COMBINATION: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 116);
pub const RESULT_TIMED_OUT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 117);
pub const RESULT_CANCELLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 118);
pub const RESULT_OUT_OF_RANGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 119);
pub const RESULT_INVALID_ENUM_VALUE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 120);
pub const RESULT_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 121);
pub const RESULT_BUSY: ResultCode = ResultCode::from_module_description(ErrorModule::Kernel, 122);
pub const RESULT_SESSION_CLOSED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 123);
pub const RESULT_INVALID_STATE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 125);
pub const RESULT_RESERVED_USED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 126);
pub const RESULT_PORT_CLOSED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 131);
pub const RESULT_LIMIT_REACHED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 132);
pub const RESULT_RECEIVE_LIST_BROKEN: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 258);
pub const RESULT_OUT_OF_ADDRESS_SPACE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 259);
pub const RESULT_MESSAGE_TOO_LARGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 260);
pub const RESULT_INVALID_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::Kernel, 519);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::result::RESULT_SUCCESS;

    #[test]
    fn test_result_codes_are_not_success() {
        assert_ne!(RESULT_INVALID_HANDLE, RESULT_SUCCESS);
        assert_ne!(RESULT_NOT_IMPLEMENTED, RESULT_SUCCESS);
        assert_ne!(RESULT_INVALID_SIZE, RESULT_SUCCESS);
    }

    #[test]
    fn test_result_codes_are_distinct() {
        assert_ne!(RESULT_INVALID_HANDLE, RESULT_INVALID_SIZE);
        assert_ne!(RESULT_INVALID_ADDRESS, RESULT_INVALID_CURRENT_MEMORY);
    }
}

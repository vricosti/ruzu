// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/errors.h

use common::error::module;
use common::ResultCode;

pub const RESULT_PATH_NOT_FOUND: ResultCode = ResultCode::new(module::FS, 1);
pub const RESULT_PATH_ALREADY_EXISTS: ResultCode = ResultCode::new(module::FS, 2);
pub const RESULT_UNSUPPORTED_SDK_VERSION: ResultCode = ResultCode::new(module::FS, 50);
pub const RESULT_PARTITION_NOT_FOUND: ResultCode = ResultCode::new(module::FS, 1001);
pub const RESULT_TARGET_NOT_FOUND: ResultCode = ResultCode::new(module::FS, 1002);
pub const RESULT_PORT_SD_CARD_NO_DEVICE: ResultCode = ResultCode::new(module::FS, 2001);
pub const RESULT_NOT_IMPLEMENTED: ResultCode = ResultCode::new(module::FS, 3001);
pub const RESULT_UNSUPPORTED_VERSION: ResultCode = ResultCode::new(module::FS, 3002);
pub const RESULT_OUT_OF_RANGE: ResultCode = ResultCode::new(module::FS, 3005);
pub const RESULT_ALLOCATION_MEMORY_FAILED_IN_FILE_SYSTEM_BUDDY_HEAP_A: ResultCode =
    ResultCode::new(module::FS, 3294);
pub const RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I: ResultCode =
    ResultCode::new(module::FS, 3341);
pub const RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_READER_A: ResultCode =
    ResultCode::new(module::FS, 3363);
pub const RESULT_ALLOCATION_MEMORY_FAILED_IN_AES_CTR_COUNTER_EXTENDED_STORAGE_A: ResultCode =
    ResultCode::new(module::FS, 3399);
pub const RESULT_ALLOCATION_MEMORY_FAILED_IN_INTEGRITY_ROM_FS_STORAGE_A: ResultCode =
    ResultCode::new(module::FS, 3412);
pub const RESULT_ALLOCATION_MEMORY_FAILED_MAKE_UNIQUE: ResultCode =
    ResultCode::new(module::FS, 3422);
pub const RESULT_ALLOCATION_MEMORY_FAILED_ALLOCATE_SHARED: ResultCode =
    ResultCode::new(module::FS, 3423);
pub const RESULT_INVALID_AES_CTR_COUNTER_EXTENDED_ENTRY_OFFSET: ResultCode =
    ResultCode::new(module::FS, 4012);
pub const RESULT_INDIRECT_STORAGE_CORRUPTED: ResultCode = ResultCode::new(module::FS, 4021);
pub const RESULT_INVALID_INDIRECT_ENTRY_OFFSET: ResultCode = ResultCode::new(module::FS, 4022);
pub const RESULT_INVALID_INDIRECT_ENTRY_STORAGE_INDEX: ResultCode =
    ResultCode::new(module::FS, 4023);
pub const RESULT_INVALID_INDIRECT_STORAGE_SIZE: ResultCode = ResultCode::new(module::FS, 4024);
pub const RESULT_INVALID_BUCKET_TREE_SIGNATURE: ResultCode = ResultCode::new(module::FS, 4032);
pub const RESULT_INVALID_BUCKET_TREE_ENTRY_COUNT: ResultCode = ResultCode::new(module::FS, 4033);
pub const RESULT_INVALID_BUCKET_TREE_NODE_ENTRY_COUNT: ResultCode =
    ResultCode::new(module::FS, 4034);
pub const RESULT_INVALID_BUCKET_TREE_NODE_OFFSET: ResultCode = ResultCode::new(module::FS, 4035);
pub const RESULT_INVALID_BUCKET_TREE_ENTRY_OFFSET: ResultCode = ResultCode::new(module::FS, 4036);
pub const RESULT_INVALID_BUCKET_TREE_ENTRY_SET_OFFSET: ResultCode =
    ResultCode::new(module::FS, 4037);
pub const RESULT_INVALID_BUCKET_TREE_NODE_INDEX: ResultCode = ResultCode::new(module::FS, 4038);
pub const RESULT_INVALID_BUCKET_TREE_VIRTUAL_OFFSET: ResultCode = ResultCode::new(module::FS, 4039);
pub const RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_TYPE: ResultCode =
    ResultCode::new(module::FS, 4084);
pub const RESULT_ROM_NCA_INVALID_INTEGRITY_LAYER_INFO_OFFSET: ResultCode =
    ResultCode::new(module::FS, 4085);
pub const RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_DATA_SIZE: ResultCode =
    ResultCode::new(module::FS, 4086);
pub const RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_DATA_OFFSET: ResultCode =
    ResultCode::new(module::FS, 4087);
pub const RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_DATA_HASH: ResultCode =
    ResultCode::new(module::FS, 4088);
pub const RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_TYPE: ResultCode =
    ResultCode::new(module::FS, 4089);
pub const RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_SIZE: ResultCode =
    ResultCode::new(module::FS, 4090);
pub const RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_OFFSET: ResultCode =
    ResultCode::new(module::FS, 4091);
// Note: upstream has duplicate error code 4091 for both offset and hash variants
pub const RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_HASH: ResultCode =
    ResultCode::new(module::FS, 4091);
pub const RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B: ResultCode = ResultCode::new(module::FS, 4509);
pub const RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_C: ResultCode = ResultCode::new(module::FS, 4510);
pub const RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D: ResultCode = ResultCode::new(module::FS, 4511);
pub const RESULT_INVALID_NCA_SIGNATURE: ResultCode = ResultCode::new(module::FS, 4517);
pub const RESULT_NCA_FS_HEADER_HASH_VERIFICATION_FAILED: ResultCode =
    ResultCode::new(module::FS, 4520);
pub const RESULT_INVALID_NCA_KEY_INDEX: ResultCode = ResultCode::new(module::FS, 4521);
pub const RESULT_INVALID_NCA_FS_HEADER_HASH_TYPE: ResultCode = ResultCode::new(module::FS, 4522);
pub const RESULT_INVALID_NCA_FS_HEADER_ENCRYPTION_TYPE: ResultCode =
    ResultCode::new(module::FS, 4523);
pub const RESULT_INVALID_NCA_PATCH_INFO_INDIRECT_SIZE: ResultCode =
    ResultCode::new(module::FS, 4524);
pub const RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_SIZE: ResultCode =
    ResultCode::new(module::FS, 4525);
pub const RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_OFFSET: ResultCode =
    ResultCode::new(module::FS, 4526);
pub const RESULT_INVALID_NCA_HEADER: ResultCode = ResultCode::new(module::FS, 4528);
pub const RESULT_INVALID_NCA_FS_HEADER: ResultCode = ResultCode::new(module::FS, 4529);
pub const RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_E: ResultCode = ResultCode::new(module::FS, 4530);
pub const RESULT_INVALID_HIERARCHICAL_SHA256_BLOCK_SIZE: ResultCode =
    ResultCode::new(module::FS, 4532);
pub const RESULT_INVALID_HIERARCHICAL_SHA256_LAYER_COUNT: ResultCode =
    ResultCode::new(module::FS, 4533);
pub const RESULT_HIERARCHICAL_SHA256_BASE_STORAGE_TOO_LARGE: ResultCode =
    ResultCode::new(module::FS, 4534);
pub const RESULT_HIERARCHICAL_SHA256_HASH_VERIFICATION_FAILED: ResultCode =
    ResultCode::new(module::FS, 4535);
pub const RESULT_INVALID_NCA_HIERARCHICAL_INTEGRITY_VERIFICATION_LAYER_COUNT: ResultCode =
    ResultCode::new(module::FS, 4541);
pub const RESULT_INVALID_NCA_INDIRECT_STORAGE_OUT_OF_RANGE: ResultCode =
    ResultCode::new(module::FS, 4542);
pub const RESULT_INVALID_NCA_HEADER1_SIGNATURE_KEY_GENERATION: ResultCode =
    ResultCode::new(module::FS, 4543);
pub const RESULT_INVALID_COMPRESSED_STORAGE_SIZE: ResultCode = ResultCode::new(module::FS, 4547);
pub const RESULT_INVALID_NCA_META_DATA_HASH_DATA_SIZE: ResultCode =
    ResultCode::new(module::FS, 4548);
pub const RESULT_INVALID_NCA_META_DATA_HASH_DATA_HASH: ResultCode =
    ResultCode::new(module::FS, 4549);
pub const RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_A: ResultCode = ResultCode::new(module::FS, 5324);
pub const RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_B: ResultCode = ResultCode::new(module::FS, 5325);
pub const RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_C: ResultCode = ResultCode::new(module::FS, 5326);
pub const RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_D: ResultCode = ResultCode::new(module::FS, 5327);
pub const RESULT_UNEXPECTED_IN_PATH_A: ResultCode = ResultCode::new(module::FS, 5328);
pub const RESULT_INVALID_ARGUMENT: ResultCode = ResultCode::new(module::FS, 6001);
pub const RESULT_INVALID_PATH: ResultCode = ResultCode::new(module::FS, 6002);
pub const RESULT_TOO_LONG_PATH: ResultCode = ResultCode::new(module::FS, 6003);
pub const RESULT_INVALID_CHARACTER: ResultCode = ResultCode::new(module::FS, 6004);
pub const RESULT_INVALID_PATH_FORMAT: ResultCode = ResultCode::new(module::FS, 6005);
pub const RESULT_DIRECTORY_UNOBTAINABLE: ResultCode = ResultCode::new(module::FS, 6006);
pub const RESULT_NOT_NORMALIZED: ResultCode = ResultCode::new(module::FS, 6007);
pub const RESULT_INVALID_OFFSET: ResultCode = ResultCode::new(module::FS, 6061);
pub const RESULT_INVALID_SIZE: ResultCode = ResultCode::new(module::FS, 6062);
pub const RESULT_NULLPTR_ARGUMENT: ResultCode = ResultCode::new(module::FS, 6063);
pub const RESULT_INVALID_OPEN_MODE: ResultCode = ResultCode::new(module::FS, 6072);
pub const RESULT_FILE_EXTENSION_WITHOUT_OPEN_MODE_ALLOW_APPEND: ResultCode =
    ResultCode::new(module::FS, 6201);
pub const RESULT_READ_NOT_PERMITTED: ResultCode = ResultCode::new(module::FS, 6202);
pub const RESULT_WRITE_NOT_PERMITTED: ResultCode = ResultCode::new(module::FS, 6203);
pub const RESULT_UNSUPPORTED_SET_SIZE_FOR_INDIRECT_STORAGE: ResultCode =
    ResultCode::new(module::FS, 6325);
pub const RESULT_UNSUPPORTED_WRITE_FOR_COMPRESSED_STORAGE: ResultCode =
    ResultCode::new(module::FS, 6387);
pub const RESULT_UNSUPPORTED_OPERATE_RANGE_FOR_COMPRESSED_STORAGE: ResultCode =
    ResultCode::new(module::FS, 6388);
pub const RESULT_PERMISSION_DENIED: ResultCode = ResultCode::new(module::FS, 6400);
pub const RESULT_BUFFER_ALLOCATION_FAILED: ResultCode = ResultCode::new(module::FS, 6705);

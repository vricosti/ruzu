//! Port of zuyu/src/core/hle/service/filesystem/fsp/fsp_srv.h and fsp_srv.cpp
//!
//! FSP_SRV service ("fsp-srv").

/// Port of Service::FileSystem::AccessLogVersion
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AccessLogVersion {
    V7_0_0 = 2,
}

impl AccessLogVersion {
    pub const LATEST: Self = Self::V7_0_0;
}

/// Port of Service::FileSystem::AccessLogMode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AccessLogMode {
    None = 0,
    Log = 1,
    SdCard = 2,
}

/// IPC command table for FSP_SRV ("fsp-srv"):
///
/// | Cmd  | Name                                                          |
/// |------|---------------------------------------------------------------|
/// | 0    | OpenFileSystem                                                |
/// | 1    | SetCurrentProcess                                             |
/// | 2    | OpenDataFileSystemByCurrentProcess                             |
/// | 7    | OpenFileSystemWithPatch                                       |
/// | 8    | OpenFileSystemWithId                                          |
/// | 9    | OpenDataFileSystemByApplicationId                              |
/// | 11   | OpenBisFileSystem                                             |
/// | 12   | OpenBisStorage                                                |
/// | 13   | InvalidateBisCache                                            |
/// | 17   | OpenHostFileSystem                                            |
/// | 18   | OpenSdCardFileSystem                                          |
/// | 19   | FormatSdCardFileSystem                                        |
/// | 21   | DeleteSaveDataFileSystem                                      |
/// | 22   | CreateSaveDataFileSystem                                      |
/// | 23   | CreateSaveDataFileSystemBySystemSaveDataId                     |
/// | 24   | RegisterSaveDataFileSystemAtomicDeletion                       |
/// | 25   | DeleteSaveDataFileSystemBySaveDataSpaceId                      |
/// | 26   | FormatSdCardDryRun                                            |
/// | 27   | IsExFatSupported                                              |
/// | 28   | DeleteSaveDataFileSystemBySaveDataAttribute                    |
/// | 30   | OpenGameCardStorage                                           |
/// | 31   | OpenGameCardFileSystem                                        |
/// | 32   | ExtendSaveDataFileSystem                                      |
/// | 33   | DeleteCacheStorage                                            |
/// | 34   | GetCacheStorageSize                                           |
/// | 35   | CreateSaveDataFileSystemByHashSalt                             |
/// | 36   | OpenHostFileSystemWithOption                                   |
/// | 51   | OpenSaveDataFileSystem                                        |
/// | 52   | OpenSaveDataFileSystemBySystemSaveDataId                       |
/// | 53   | OpenReadOnlySaveDataFileSystem                                 |
/// | 57   | ReadSaveDataFileSystemExtraDataBySaveDataSpaceId                |
/// | 58   | ReadSaveDataFileSystemExtraData                                 |
/// | 59   | WriteSaveDataFileSystemExtraData                                |
/// | 60   | OpenSaveDataInfoReader                                          |
/// | 61   | OpenSaveDataInfoReaderBySaveDataSpaceId                         |
/// | 62   | OpenSaveDataInfoReaderOnlyCacheStorage                          |
/// | 64   | OpenSaveDataInternalStorageFileSystem                           |
/// | 65   | UpdateSaveDataMacForDebug                                       |
/// | 66   | WriteSaveDataFileSystemExtraData2                                |
/// | 67   | FindSaveDataWithFilter                                           |
/// | 68   | OpenSaveDataInfoReaderBySaveDataFilter                           |
/// | 69   | ReadSaveDataFileSystemExtraDataBySaveDataAttribute               |
/// | 70   | WriteSaveDataFileSystemExtraDataWithMaskBySaveDataAttribute      |
/// | 71   | ReadSaveDataFileSystemExtraDataWithMaskBySaveDataAttribute       |
/// | 80   | OpenSaveDataMetaFile                                             |
/// | 81   | OpenSaveDataTransferManager                                      |
/// | 82   | OpenSaveDataTransferManagerVersion2                               |
/// | 83   | OpenSaveDataTransferProhibiter                                    |
/// | 84   | ListApplicationAccessibleSaveDataOwnerId                          |
/// | 85   | OpenSaveDataTransferManagerForSaveDataRepair                       |
/// | 86   | OpenSaveDataMover                                                  |
/// | 87   | OpenSaveDataTransferManagerForRepair                               |
/// | 100  | OpenImageDirectoryFileSystem                                       |
/// | 101  | OpenBaseFileSystem                                                  |
/// | 102  | FormatBaseFileSystem                                                |
/// | 110  | OpenContentStorageFileSystem                                        |
/// | 120  | OpenCloudBackupWorkStorageFileSystem                                |
/// | 130  | OpenCustomStorageFileSystem                                          |
/// | 200  | OpenDataStorageByCurrentProcess                                      |
/// | 201  | OpenDataStorageByProgramId                                            |
/// | 202  | OpenDataStorageByDataId                                                |
/// | 203  | OpenPatchDataStorageByCurrentProcess                                   |
/// | 204  | OpenDataFileSystemByProgramIndex                                       |
/// | 205  | OpenDataStorageWithProgramIndex                                        |
/// | 206  | OpenDataStorageByPath                                                  |
/// | 400  | OpenDeviceOperator                                                     |
/// | 500  | OpenSdCardDetectionEventNotifier                                       |
/// | 501  | OpenGameCardDetectionEventNotifier                                     |
/// | 510  | OpenSystemDataUpdateEventNotifier                                      |
/// | 511  | NotifySystemDataUpdateEvent                                            |
/// | 520  | SimulateGameCardDetectionEvent                                         |
/// | 600-617 | (various utility commands)                                          |
/// | 620  | SetSdCardEncryptionSeed                                                |
/// | 630-631 | SD card accessibility                                               |
/// | 640  | IsSignedSystemPartitionOnSdCardValid                                   |
/// | 700-720 | Access failure resolver                                             |
/// | 800  | GetAndClearFileSystemProxyErrorInfo                                    |
/// | 810  | RegisterProgramIndexMapInfo                                            |
/// | 1000-1019 | Debug/development commands                                       |
/// | 1003 | DisableAutoSaveDataCreation                                            |
/// | 1004 | SetGlobalAccessLogMode                                                 |
/// | 1005 | GetGlobalAccessLogMode                                                 |
/// | 1006 | OutputAccessLogToSdCard                                                |
/// | 1011 | GetProgramIndexForAccessLog                                            |
/// | 1016 | FlushAccessLogOnSdCard                                                 |
/// | 1100 | OverrideSaveDataTransferTokenSignVerificationKey                        |
/// | 1110 | CorruptSaveDataFileSystemBySaveDataSpaceId2                             |
/// | 1200 | OpenMultiCommitManager                                                 |
/// | 1300 | OpenBisWiper                                                           |
pub struct FspSrv {
    _current_process_id: u64,
    _access_log_program_index: u32,
    _access_log_mode: AccessLogMode,
    _program_id: u64,
}

impl FspSrv {
    pub fn new() -> Self {
        Self {
            _current_process_id: 0,
            _access_log_program_index: 0,
            _access_log_mode: AccessLogMode::None,
            _program_id: 0,
        }
    }
}

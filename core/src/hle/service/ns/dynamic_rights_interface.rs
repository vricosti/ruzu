// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/dynamic_rights_interface.cpp/.h

pub const IDYNAMIC_RIGHTS_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, false, "RequestApplicationRightsOnServer"),
    (1, false, "RequestAssignRights"),
    (4, false, "DeprecatedRequestAssignRightsToResume"),
    (5, true, "VerifyActivatedRightsOwners"),
    (6, false, "DeprecatedGetApplicationRightsStatus"),
    (7, false, "RequestPrefetchForDynamicRights"),
    (8, false, "GetDynamicRightsState"),
    (9, false, "RequestApplicationRightsOnServerToResume"),
    (10, false, "RequestAssignRightsToResume"),
    (11, false, "GetActivatedRightsUsers"),
    (12, false, "GetApplicationRightsStatus"),
    (13, true, "GetRunningApplicationStatus"),
    (14, false, "SelectApplicationLicense"),
    (15, false, "RequestContentsAuthorizationToken"),
    (16, false, "QualifyUser"),
    (17, false, "QualifyUserWithProcessId"),
    (18, true, "NotifyApplicationRightsCheckStart"),
    (19, false, "UpdateUserList"),
    (20, false, "IsRightsLostUser"),
    (21, false, "SetRequiredAddOnContentsOnContentsAvailabilityTransition"),
    (22, false, "GetLimitedApplicationLicense"),
    (23, false, "GetLimitedApplicationLicenseUpgradableEvent"),
    (24, false, "NotifyLimitedApplicationLicenseUpgradableEventForDebug"),
    (25, false, "RequestProceedDynamicRightsState"),
];

pub fn notify_application_rights_check_start() {
    log::warn!("(STUBBED) IDynamicRightsInterface::NotifyApplicationRightsCheckStart called");
}

pub fn get_running_application_status(_rights_handle: u64) -> u32 {
    log::warn!("(STUBBED) IDynamicRightsInterface::GetRunningApplicationStatus called");
    0
}

pub fn verify_activated_rights_owners(_rights_handle: u64) {
    log::warn!("(STUBBED) IDynamicRightsInterface::VerifyActivatedRightsOwners called");
}

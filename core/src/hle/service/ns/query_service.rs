// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/query_service.cpp/.h

use super::ns_types::{PlayStatistics, Uid};

pub const IQUERY_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, false, "QueryAppletEvent"),
    (1, false, "QueryPlayStatistics"),
    (2, false, "QueryPlayStatisticsByUserAccountId"),
    (3, false, "QueryPlayStatisticsByNetworkServiceAccountId"),
    (4, false, "QueryPlayStatisticsByApplicationId"),
    (5, true, "QueryPlayStatisticsByApplicationIdAndUserAccountId"),
    (6, false, "QueryPlayStatisticsByApplicationIdAndNetworkServiceAccountId"),
    (7, false, "QueryLastPlayTimeV0"),
    (8, false, "QueryPlayEvent"),
    (9, false, "GetAvailablePlayEventRange"),
    (10, false, "QueryAccountEvent"),
    (11, false, "QueryAccountPlayEvent"),
    (12, false, "GetAvailableAccountPlayEventRange"),
    (13, false, "QueryApplicationPlayStatisticsForSystemV0"),
    (14, false, "QueryRecentlyPlayedApplication"),
    (15, false, "GetRecentlyPlayedApplicationUpdateEvent"),
    (16, false, "QueryApplicationPlayStatisticsByUserAccountIdForSystemV0"),
    (17, false, "QueryLastPlayTime"),
    (18, false, "QueryApplicationPlayStatisticsForSystem"),
    (19, false, "QueryApplicationPlayStatisticsByUserAccountIdForSystem"),
];

pub fn query_play_statistics_by_application_id_and_user_account_id(
    _unknown: bool,
    application_id: u64,
    _account_id: &Uid,
) -> PlayStatistics {
    log::warn!(
        "(STUBBED) IQueryService::QueryPlayStatisticsByApplicationIdAndUserAccountId called, \
         application_id={:016X}",
        application_id
    );
    PlayStatistics {
        application_id,
        total_launches: 1,
        ..Default::default()
    }
}

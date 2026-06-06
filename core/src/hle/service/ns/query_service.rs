// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/query_service.cpp/.h

use std::collections::BTreeMap;

use super::ns_types::{PlayStatistics, Uid};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{CmifRequest, CmifResponse};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{FunctionInfo, ServiceFramework};

pub const IQUERY_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, false, "QueryAppletEvent"),
    (1, false, "QueryPlayStatistics"),
    (2, false, "QueryPlayStatisticsByUserAccountId"),
    (3, false, "QueryPlayStatisticsByNetworkServiceAccountId"),
    (4, false, "QueryPlayStatisticsByApplicationId"),
    (
        5,
        true,
        "QueryPlayStatisticsByApplicationIdAndUserAccountId",
    ),
    (
        6,
        false,
        "QueryPlayStatisticsByApplicationIdAndNetworkServiceAccountId",
    ),
    (7, false, "QueryLastPlayTimeV0"),
    (8, false, "QueryPlayEvent"),
    (9, false, "GetAvailablePlayEventRange"),
    (10, false, "QueryAccountEvent"),
    (11, false, "QueryAccountPlayEvent"),
    (12, false, "GetAvailableAccountPlayEventRange"),
    (13, false, "QueryApplicationPlayStatisticsForSystemV0"),
    (14, false, "QueryRecentlyPlayedApplication"),
    (15, false, "GetRecentlyPlayedApplicationUpdateEvent"),
    (
        16,
        false,
        "QueryApplicationPlayStatisticsByUserAccountIdForSystemV0",
    ),
    (17, false, "QueryLastPlayTime"),
    (18, false, "QueryApplicationPlayStatisticsForSystem"),
    (
        19,
        false,
        "QueryApplicationPlayStatisticsByUserAccountIdForSystem",
    ),
];

/// `pdm:qry` query service.
///
/// Corresponds to upstream `Service::NS::IQueryService`.
pub struct IQueryService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IQueryService {
    pub fn new() -> Self {
        let handlers = IQUERY_SERVICE_COMMANDS
            .iter()
            .map(|&(id, has_handler, name)| {
                let callback = if has_handler {
                    Some(
                        Self::query_play_statistics_by_application_id_and_user_account_id_handler
                            as fn(&dyn ServiceFramework, &mut HLERequestContext),
                    )
                } else {
                    None
                };
                (id, FunctionInfo::new(id, callback, name))
            })
            .collect();
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn query_play_statistics_by_application_id_and_user_account_id_handler(
        _service: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut request = CmifRequest::new(ctx);
        let unknown = request.raw::<u8>() != 0;
        request.align_for::<u64>();
        let application_id = request.u64();
        let account_id = request.raw::<Uid>();

        let play_statistics = query_play_statistics_by_application_id_and_user_account_id(
            unknown,
            application_id,
            &account_id,
        );

        let mut response = CmifResponse::new(
            ctx,
            2 + (core::mem::size_of::<PlayStatistics>() / core::mem::size_of::<u32>()) as u32,
            0,
            0,
        );
        response.push_result(RESULT_SUCCESS);
        response.push_raw(&play_statistics);
    }
}

impl SessionRequestHandler for IQueryService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "pdm:qry"
    }
}

impl ServiceFramework for IQueryService {
    fn get_service_name(&self) -> &str {
        "pdm:qry"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

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

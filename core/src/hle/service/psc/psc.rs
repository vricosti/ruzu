// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/psc.cpp/.h
//!
//! LoopProcess registers the following named services:
//!   psc:c     -> IPmControl
//!   psc:m     -> IPmService
//!   ovln:rcv  -> IReceiverService
//!   ovln:snd  -> ISenderService
//!   time:m    -> Time::ServiceManager
//!   time:su   -> Time::StaticService
//!   time:al   -> Time::IAlarmService

pub const PSC_SERVICE_NAMES: &[&str] = &[
    "psc:c", "psc:m", "ovln:rcv", "ovln:snd", "time:m", "time:su", "time:al",
];

// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/
//! Upstream files:
//!   - bcat.h / bcat.cpp
//!   - bcat_result.h
//!   - bcat_types.h
//!   - bcat_util.h
//!   - bcat_service.h / bcat_service.cpp
//!   - service_creator.h / service_creator.cpp
//!   - delivery_cache_storage_service.h / delivery_cache_storage_service.cpp
//!   - delivery_cache_file_service.h / delivery_cache_file_service.cpp
//!   - delivery_cache_directory_service.h / delivery_cache_directory_service.cpp
//!   - delivery_cache_progress_service.h / delivery_cache_progress_service.cpp
//!   - backend/backend.h / backend/backend.cpp
//!   - news/ (service_creator, news_service, news_database_service, news_data_service,
//!            newly_arrived_event_holder, overwrite_event_holder)

pub mod backend;
pub mod bcat;
pub mod bcat_result;
pub mod bcat_service;
pub mod bcat_types;
pub mod bcat_util;
pub mod delivery_cache_directory_service;
pub mod delivery_cache_file_service;
pub mod delivery_cache_progress_service;
pub mod delivery_cache_storage_service;
pub mod news;
pub mod service_creator;

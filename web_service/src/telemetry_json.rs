// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/telemetry_json.h and telemetry_json.cpp
//!
//! Implementation of `VisitorInterface` that serializes telemetry into JSON
//! and submits it to the web service.

use std::collections::HashMap;
use std::time::Duration;

use crate::web_backend::Client;

// ---------------------------------------------------------------------------
// FieldType (mirrors common::telemetry::FieldType)
// ---------------------------------------------------------------------------

/// Telemetry field types.
/// Maps to C++ `Common::Telemetry::FieldType`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum FieldType {
    None = 0,
    App,
    Session,
    Performance,
    UserConfig,
    UserSystem,
    UserFeedback,
}

// ---------------------------------------------------------------------------
// TelemetryJson
// ---------------------------------------------------------------------------

/// Internal implementation data.
/// Maps to C++ `WebService::TelemetryJson::Impl`.
struct TelemetryJsonImpl {
    /// Sections indexed by FieldType.
    sections: HashMap<u8, HashMap<String, String>>,
    host: String,
    username: String,
    token: String,
}

impl TelemetryJsonImpl {
    fn new(host: String, username: String, token: String) -> Self {
        Self {
            sections: HashMap::new(),
            host,
            username,
            token,
        }
    }

    fn serialize(&mut self, field_type: FieldType, name: &str, value: &str) {
        self.sections
            .entry(field_type as u8)
            .or_default()
            .insert(name.to_string(), value.to_string());
    }

    fn top_section(&self) -> &HashMap<String, String> {
        static EMPTY: std::sync::LazyLock<HashMap<String, String>> =
            std::sync::LazyLock::new(HashMap::new);
        self.sections.get(&(FieldType::None as u8)).unwrap_or(&EMPTY)
    }

    fn serialize_section(&mut self, field_type: FieldType, name: &str) {
        // NOTE: In C++ this nests the section into the top-level JSON object.
        // Here we store a placeholder string indicating the section was serialized.
        let section_data = self
            .sections
            .get(&(field_type as u8))
            .cloned()
            .unwrap_or_default();
        let value = format!("{:?}", section_data);
        self.sections
            .entry(FieldType::None as u8)
            .or_default()
            .insert(name.to_string(), value);
    }
}

/// Serializes telemetry into JSON and submits it to the web service.
/// Maps to C++ `WebService::TelemetryJson`.
pub struct TelemetryJson {
    inner: TelemetryJsonImpl,
}

impl TelemetryJson {
    pub fn new(host: String, username: String, token: String) -> Self {
        Self {
            inner: TelemetryJsonImpl::new(host, username, token),
        }
    }

    // -----------------------------------------------------------------------
    // Visit methods (one per type, matching C++ overloads)
    // -----------------------------------------------------------------------

    pub fn visit_bool(&mut self, field_type: FieldType, name: &str, value: bool) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_f64(&mut self, field_type: FieldType, name: &str, value: f64) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_f32(&mut self, field_type: FieldType, name: &str, value: f32) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_u8(&mut self, field_type: FieldType, name: &str, value: u8) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_u16(&mut self, field_type: FieldType, name: &str, value: u16) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_u32(&mut self, field_type: FieldType, name: &str, value: u32) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_u64(&mut self, field_type: FieldType, name: &str, value: u64) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_i8(&mut self, field_type: FieldType, name: &str, value: i8) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_i16(&mut self, field_type: FieldType, name: &str, value: i16) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_i32(&mut self, field_type: FieldType, name: &str, value: i32) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_i64(&mut self, field_type: FieldType, name: &str, value: i64) {
        self.inner
            .serialize(field_type, name, &value.to_string());
    }

    pub fn visit_string(&mut self, field_type: FieldType, name: &str, value: &str) {
        self.inner.serialize(field_type, name, value);
    }

    pub fn visit_duration(&mut self, field_type: FieldType, name: &str, value: Duration) {
        self.inner
            .serialize(field_type, name, &value.as_micros().to_string());
    }

    // -----------------------------------------------------------------------
    // Complete / SubmitTestcase
    // -----------------------------------------------------------------------

    /// Serializes and submits the telemetry data.
    pub fn complete(&mut self) {
        self.inner.serialize_section(FieldType::App, "App");
        self.inner.serialize_section(FieldType::Session, "Session");
        self.inner
            .serialize_section(FieldType::Performance, "Performance");
        self.inner
            .serialize_section(FieldType::UserConfig, "UserConfig");
        self.inner
            .serialize_section(FieldType::UserSystem, "UserSystem");

        // NOTE: Would POST to /telemetry via a detached task. Stubbed.
        let _content = format!("{:?}", self.inner.top_section());
        log::warn!("TelemetryJson::complete: HTTP client not implemented; telemetry data not submitted");
    }

    /// Submits a testcase.
    pub fn submit_testcase(&mut self) -> bool {
        self.inner.serialize_section(FieldType::App, "App");
        self.inner.serialize_section(FieldType::Session, "Session");
        self.inner
            .serialize_section(FieldType::UserFeedback, "UserFeedback");
        self.inner
            .serialize_section(FieldType::UserSystem, "UserSystem");
        self.inner
            .serialize_section(FieldType::UserConfig, "UserConfig");

        // NOTE: Would POST to /gamedb/testcase. Stubbed.
        log::warn!("TelemetryJson::submit_testcase: HTTP client not implemented; testcase not submitted");
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_telemetry_json_visit() {
        let mut tj = TelemetryJson::new(
            "https://example.com".to_string(),
            "user".to_string(),
            "token".to_string(),
        );
        tj.visit_u32(FieldType::App, "test_field", 42);
        tj.visit_string(FieldType::Session, "name", "test");

        assert!(tj
            .inner
            .sections
            .get(&(FieldType::App as u8))
            .unwrap()
            .contains_key("test_field"));
    }
}

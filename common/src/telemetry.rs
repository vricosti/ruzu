// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/telemetry.h and zuyu/src/common/telemetry.cpp
//!
//! Visitor pattern for telemetry field collection.
//! Provides a type-safe way to collect and visit various telemetry data fields.

use std::collections::BTreeMap;
use std::time::Duration;

/// Field type, used for grouping fields together in the final submitted telemetry log.
/// Corresponds to `Common::Telemetry::FieldType` in C++.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum FieldType {
    /// No specified field group
    None = 0,
    /// Application fields (e.g. version, branch, etc.)
    App,
    /// Emulated session fields (e.g. title ID, log, etc.)
    Session,
    /// Emulated performance (e.g. fps, emulated CPU speed, etc.)
    Performance,
    /// User submitted feedback (e.g. star rating, user notes, etc.)
    UserFeedback,
    /// User configuration fields (e.g. emulated CPU core, renderer, etc.)
    UserConfig,
    /// User system information (e.g. host CPU type, RAM, etc.)
    UserSystem,
}

/// Enum representing all possible telemetry field value types.
/// This replaces the C++ template `Field<T>` + virtual dispatch pattern
/// with a Rust enum for type-safe value storage.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    Bool(bool),
    F64(f64),
    F32(f32),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    String(String),
    StaticStr(&'static str),
    Duration(Duration),
}

/// A telemetry data field, i.e. a unit of data that gets logged and submitted
/// to a telemetry service.
///
/// Corresponds to `Common::Telemetry::Field<T>` in C++.
#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    field_type: FieldType,
    value: FieldValue,
}

impl Field {
    /// Create a new telemetry field.
    pub fn new(field_type: FieldType, name: impl Into<String>, value: FieldValue) -> Self {
        Self {
            name: name.into(),
            field_type,
            value,
        }
    }

    /// Gets the name of this field.
    /// Corresponds to `FieldInterface::GetName()`.
    pub fn get_name(&self) -> &str {
        &self.name
    }

    /// Returns the type of the field.
    /// Corresponds to `Field<T>::GetType()`.
    pub fn get_type(&self) -> FieldType {
        self.field_type
    }

    /// Returns the value of the field.
    /// Corresponds to `Field<T>::GetValue()`.
    pub fn get_value(&self) -> &FieldValue {
        &self.value
    }

    /// Accept method for the visitor pattern.
    /// Corresponds to `Field<T>::Accept(VisitorInterface&)`.
    pub fn accept(&self, visitor: &mut dyn VisitorInterface) {
        visitor.visit(self);
    }
}

impl PartialEq for Field {
    fn eq(&self, other: &Self) -> bool {
        self.field_type == other.field_type && self.name == other.name && self.value == other.value
    }
}

/// Collection of data fields that have been logged.
///
/// Corresponds to `Common::Telemetry::FieldCollection` in C++.
#[derive(Debug, Default)]
pub struct FieldCollection {
    fields: BTreeMap<String, Field>,
}

impl FieldCollection {
    /// Create a new empty field collection.
    pub fn new() -> Self {
        Self::default()
    }

    /// Accept method for the visitor pattern, visits each field in the collection.
    /// Corresponds to `FieldCollection::Accept(VisitorInterface&)`.
    pub fn accept(&self, visitor: &mut dyn VisitorInterface) {
        for field in self.fields.values() {
            field.accept(visitor);
        }
    }

    /// Creates a new field and adds it to the field collection.
    /// Corresponds to the template `FieldCollection::AddField<T>`.
    pub fn add_field(
        &mut self,
        field_type: FieldType,
        name: impl Into<String>,
        value: impl Into<FieldValue>,
    ) {
        let name = name.into();
        let field = Field::new(field_type, name.clone(), value.into());
        self.fields.insert(name, field);
    }

    /// Adds an existing field to the collection.
    /// Corresponds to `FieldCollection::AddField(std::unique_ptr<FieldInterface>)`.
    pub fn add_field_entry(&mut self, field: Field) {
        self.fields.insert(field.name.clone(), field);
    }
}

// ── FieldValue From implementations ──

impl From<bool> for FieldValue {
    fn from(v: bool) -> Self {
        FieldValue::Bool(v)
    }
}

impl From<f64> for FieldValue {
    fn from(v: f64) -> Self {
        FieldValue::F64(v)
    }
}

impl From<f32> for FieldValue {
    fn from(v: f32) -> Self {
        FieldValue::F32(v)
    }
}

impl From<u8> for FieldValue {
    fn from(v: u8) -> Self {
        FieldValue::U8(v)
    }
}

impl From<u16> for FieldValue {
    fn from(v: u16) -> Self {
        FieldValue::U16(v)
    }
}

impl From<u32> for FieldValue {
    fn from(v: u32) -> Self {
        FieldValue::U32(v)
    }
}

impl From<u64> for FieldValue {
    fn from(v: u64) -> Self {
        FieldValue::U64(v)
    }
}

impl From<i8> for FieldValue {
    fn from(v: i8) -> Self {
        FieldValue::I8(v)
    }
}

impl From<i16> for FieldValue {
    fn from(v: i16) -> Self {
        FieldValue::I16(v)
    }
}

impl From<i32> for FieldValue {
    fn from(v: i32) -> Self {
        FieldValue::I32(v)
    }
}

impl From<i64> for FieldValue {
    fn from(v: i64) -> Self {
        FieldValue::I64(v)
    }
}

impl From<String> for FieldValue {
    fn from(v: String) -> Self {
        FieldValue::String(v)
    }
}

impl From<&'static str> for FieldValue {
    fn from(v: &'static str) -> Self {
        FieldValue::StaticStr(v)
    }
}

impl From<Duration> for FieldValue {
    fn from(v: Duration) -> Self {
        FieldValue::Duration(v)
    }
}

/// Telemetry fields visitor interface.
/// A backend to log to a web service should implement this trait.
///
/// Corresponds to `Common::Telemetry::VisitorInterface` in C++.
///
/// In C++, there is one `Visit` overload per type. In Rust, we use a single
/// `visit` method that receives a `Field` with the value accessible as a `FieldValue` enum.
pub trait VisitorInterface {
    /// Visit a telemetry field.
    fn visit(&mut self, field: &Field);

    /// Completion method, called once all fields have been visited.
    /// Corresponds to `VisitorInterface::Complete()`.
    fn complete(&mut self);

    /// Submit a testcase.
    /// Corresponds to `VisitorInterface::SubmitTestcase()`.
    fn submit_testcase(&mut self) -> bool;
}

/// Empty implementation of VisitorInterface that drops all fields.
/// Used when a functional backend implementation is not available.
///
/// Corresponds to `Common::Telemetry::NullVisitor` in C++.
pub struct NullVisitor;

impl NullVisitor {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NullVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl VisitorInterface for NullVisitor {
    fn visit(&mut self, _field: &Field) {}

    fn complete(&mut self) {}

    fn submit_testcase(&mut self) -> bool {
        false
    }
}

/// Appends build-specific information to the given FieldCollection,
/// such as branch name, revision hash, etc.
///
/// Corresponds to `Common::Telemetry::AppendBuildInfo`.
///
/// Note: In the C++ version this reads from `Common::g_scm_rev` etc.
/// In Rust we use compile-time environment variables where available.
pub fn append_build_info(fc: &mut FieldCollection) {
    // These would come from build-time environment variables in a real build.
    let scm_desc = option_env!("GIT_DESC").unwrap_or("unknown");
    let is_git_dirty = scm_desc.contains("dirty");

    fc.add_field(FieldType::App, "Git_IsDirty", is_git_dirty);
    fc.add_field(
        FieldType::App,
        "Git_Branch",
        option_env!("GIT_BRANCH").unwrap_or("unknown"),
    );
    fc.add_field(
        FieldType::App,
        "Git_Revision",
        option_env!("GIT_REV").unwrap_or("unknown"),
    );
    fc.add_field(
        FieldType::App,
        "BuildDate",
        option_env!("BUILD_DATE").unwrap_or("unknown"),
    );
    fc.add_field(
        FieldType::App,
        "BuildName",
        option_env!("BUILD_NAME").unwrap_or("ruzu"),
    );
}

/// Appends CPU-specific information to the given FieldCollection.
///
/// Corresponds to `Common::Telemetry::AppendCPUInfo`.
pub fn append_cpu_info(fc: &mut FieldCollection) {
    // Simplified — the C++ version reads x86_64 CPUID info.
    // In Rust, we report what we can.
    #[cfg(target_arch = "x86_64")]
    {
        fc.add_field(FieldType::UserSystem, "CPU_Model", "x86_64");
    }
    #[cfg(target_arch = "aarch64")]
    {
        fc.add_field(FieldType::UserSystem, "CPU_Model", "aarch64");
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        fc.add_field(FieldType::UserSystem, "CPU_Model", "Other");
    }
}

/// Appends OS-specific information to the given FieldCollection.
///
/// Corresponds to `Common::Telemetry::AppendOSInfo`.
pub fn append_os_info(fc: &mut FieldCollection) {
    #[cfg(target_os = "macos")]
    let platform = "Apple";
    #[cfg(target_os = "windows")]
    let platform = "Windows";
    #[cfg(target_os = "linux")]
    let platform = "Linux";
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    let platform = "Unknown";

    fc.add_field(FieldType::UserSystem, "OsPlatform", platform);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_creation() {
        let field = Field::new(FieldType::App, "TestField", FieldValue::U32(42));
        assert_eq!(field.get_name(), "TestField");
        assert_eq!(field.get_type(), FieldType::App);
        assert_eq!(*field.get_value(), FieldValue::U32(42));
    }

    #[test]
    fn test_field_equality() {
        let f1 = Field::new(FieldType::App, "Test", FieldValue::Bool(true));
        let f2 = Field::new(FieldType::App, "Test", FieldValue::Bool(true));
        let f3 = Field::new(FieldType::App, "Test", FieldValue::Bool(false));

        assert_eq!(f1, f2);
        assert_ne!(f1, f3);
    }

    #[test]
    fn test_field_collection() {
        let mut fc = FieldCollection::new();
        fc.add_field(FieldType::App, "Field1", 42u32);
        fc.add_field(FieldType::Session, "Field2", "hello");
        fc.add_field(FieldType::Performance, "Field3", true);

        // Overwrite a field
        fc.add_field(FieldType::App, "Field1", 99u32);

        let mut visitor = TestVisitor::new();
        fc.accept(&mut visitor);

        // Should have 3 fields (Field1 was overwritten, not duplicated)
        assert_eq!(visitor.visited.len(), 3);
    }

    #[test]
    fn test_null_visitor() {
        let mut visitor = NullVisitor::new();
        let field = Field::new(FieldType::App, "Test", FieldValue::Bool(true));
        field.accept(&mut visitor);
        visitor.complete();
        assert!(!visitor.submit_testcase());
    }

    #[test]
    fn test_append_build_info() {
        let mut fc = FieldCollection::new();
        append_build_info(&mut fc);

        let mut visitor = TestVisitor::new();
        fc.accept(&mut visitor);

        // Should have added several fields
        assert!(visitor.visited.len() >= 4);
    }

    #[test]
    fn test_append_os_info() {
        let mut fc = FieldCollection::new();
        append_os_info(&mut fc);

        let mut visitor = TestVisitor::new();
        fc.accept(&mut visitor);

        assert_eq!(visitor.visited.len(), 1);
        assert_eq!(visitor.visited[0].0, "OsPlatform");
    }

    struct TestVisitor {
        visited: Vec<(String, FieldValue)>,
    }

    impl TestVisitor {
        fn new() -> Self {
            Self {
                visited: Vec::new(),
            }
        }
    }

    impl VisitorInterface for TestVisitor {
        fn visit(&mut self, field: &Field) {
            self.visited
                .push((field.get_name().to_string(), field.get_value().clone()));
        }
        fn complete(&mut self) {}
        fn submit_testcase(&mut self) -> bool {
            false
        }
    }
}

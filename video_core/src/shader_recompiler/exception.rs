// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/exception.h`
//!
//! Shader recompiler exception types. In Rust, these are represented as
//! error enums rather than exception classes, used with `Result`.

use std::fmt;

/// Base shader recompiler error.
#[derive(Debug, Clone)]
pub struct ShaderException {
    message: String,
}

impl ShaderException {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn prepend(&mut self, prefix: &str) {
        self.message.insert_str(0, prefix);
    }

    pub fn append(&mut self, suffix: &str) {
        self.message.push_str(suffix);
    }
}

impl fmt::Display for ShaderException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ShaderException {}

/// Logic error in shader recompiler (programming error).
#[derive(Debug, Clone)]
pub struct LogicError(pub String);

impl LogicError {
    pub fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl fmt::Display for LogicError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for LogicError {}

/// Runtime error in shader recompiler.
#[derive(Debug, Clone)]
pub struct RuntimeError(pub String);

impl RuntimeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for RuntimeError {}

/// Not-implemented error in shader recompiler.
#[derive(Debug, Clone)]
pub struct NotImplementedException(pub String);

impl NotImplementedException {
    pub fn new(message: impl Into<String>) -> Self {
        let mut msg = message.into();
        msg.push_str(" is not implemented");
        Self(msg)
    }
}

impl fmt::Display for NotImplementedException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for NotImplementedException {}

/// Invalid argument error in shader recompiler.
#[derive(Debug, Clone)]
pub struct InvalidArgument(pub String);

impl InvalidArgument {
    pub fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl fmt::Display for InvalidArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for InvalidArgument {}

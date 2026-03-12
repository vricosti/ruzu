// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/web_backend.h and web_backend.cpp
//!
//! Provides the HTTP client for communicating with the web service.
//! The C++ version uses cpp-httplib; this Rust port stubs the HTTP layer.

use crate::web_result::{WebResult, WebResultCode};

// ---------------------------------------------------------------------------
// Constants (from web_backend.cpp)
// ---------------------------------------------------------------------------

/// API version string.
pub const API_VERSION: &str = "1";

/// HTTP timeout in seconds.
pub const TIMEOUT_SECONDS: u64 = 30;

// ---------------------------------------------------------------------------
// Client
// ---------------------------------------------------------------------------

/// HTTP client for communicating with the web service.
/// Maps to C++ `WebService::Client`.
pub struct Client {
    host: String,
    username: String,
    token: String,
    jwt: String,
}

impl Client {
    pub fn new(host: String, username: String, token: String) -> Self {
        let mut normalized_host = host;
        // Normalize host expression
        if normalized_host.ends_with('/') {
            normalized_host.pop();
        }

        Self {
            host: normalized_host,
            username,
            token,
            jwt: String::new(),
        }
    }

    /// Posts JSON to the specified path.
    pub fn post_json(&mut self, path: &str, data: &str, allow_anonymous: bool) -> WebResult {
        self.generic_request("POST", path, data, allow_anonymous, "application/json")
    }

    /// Gets JSON from the specified path.
    pub fn get_json(&mut self, path: &str, allow_anonymous: bool) -> WebResult {
        self.generic_request("GET", path, "", allow_anonymous, "application/json")
    }

    /// Deletes JSON at the specified path.
    pub fn delete_json(&mut self, path: &str, data: &str, allow_anonymous: bool) -> WebResult {
        self.generic_request("DELETE", path, data, allow_anonymous, "application/json")
    }

    /// Gets a plain string from the specified path.
    pub fn get_plain(&mut self, path: &str, allow_anonymous: bool) -> WebResult {
        self.generic_request("GET", path, "", allow_anonymous, "text/plain")
    }

    /// Gets a PNG image from the specified path.
    pub fn get_image(&mut self, path: &str, allow_anonymous: bool) -> WebResult {
        self.generic_request("GET", path, "", allow_anonymous, "image/png")
    }

    /// Requests an external JWT for the specified audience.
    pub fn get_external_jwt(&mut self, audience: &str) -> WebResult {
        self.generic_request(
            "POST",
            &format!("/jwt/external/{}", audience),
            "",
            false,
            "text/html",
        )
    }

    /// A generic function that handles POST, GET and DELETE requests.
    ///
    /// NOTE: The actual HTTP client (cpp-httplib) is not ported. This method
    /// is stubbed to return `LibError` indicating the HTTP layer is missing.
    fn generic_request(
        &mut self,
        method: &str,
        path: &str,
        _data: &str,
        allow_anonymous: bool,
        _accept: &str,
    ) -> WebResult {
        if self.jwt.is_empty() {
            self.update_jwt();
        }

        if self.jwt.is_empty() && !allow_anonymous {
            log::error!("Credentials must be provided for authenticated requests");
            return WebResult {
                result_code: WebResultCode::CredentialsMissing,
                result_string: "Credentials needed".to_string(),
                returned_data: String::new(),
            };
        }

        // NOTE: Actual HTTP request is not implemented.
        log::error!(
            "HTTP client not implemented: {} to {}{}",
            method,
            self.host,
            path
        );
        WebResult {
            result_code: WebResultCode::LibError,
            result_string: "HTTP client not implemented".to_string(),
            returned_data: String::new(),
        }
    }

    /// Retrieve a new JWT from given username and token.
    fn update_jwt(&mut self) {
        if self.username.is_empty() || self.token.is_empty() {
            return;
        }
        // NOTE: Would POST to /jwt/internal; stubbed.
        log::error!("UpdateJWT: HTTP client not implemented");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_client_normalizes_host() {
        let client = Client::new(
            "https://example.com/".to_string(),
            String::new(),
            String::new(),
        );
        assert_eq!(client.host, "https://example.com");
    }

    #[test]
    fn test_anonymous_request_returns_lib_error() {
        let mut client = Client::new(
            "https://example.com".to_string(),
            String::new(),
            String::new(),
        );
        let result = client.get_json("/test", true);
        assert_eq!(result.result_code, WebResultCode::LibError);
    }

    #[test]
    fn test_authenticated_request_without_credentials() {
        let mut client = Client::new(
            "https://example.com".to_string(),
            String::new(),
            String::new(),
        );
        let result = client.get_json("/test", false);
        assert_eq!(result.result_code, WebResultCode::CredentialsMissing);
    }
}

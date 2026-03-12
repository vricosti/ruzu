// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/verify_login.h and verify_login.cpp
//!
//! Checks if username and token are valid against the web service.

use crate::web_backend::Client;
use crate::web_result::WebResultCode;

/// Checks if username and token are valid.
///
/// # Arguments
/// * `host` - The web API URL.
/// * `username` - Username to use for authentication.
/// * `token` - Token to use for authentication.
///
/// # Returns
/// `true` if the verification succeeded.
///
/// NOTE: The HTTP client is not implemented, so this will always return
/// `false` at this time.
pub fn verify_login(host: &str, username: &str, token: &str) -> bool {
    let mut client = Client::new(host.to_string(), username.to_string(), token.to_string());
    let reply = client.get_json("/profile", false).returned_data;
    if reply.is_empty() {
        return false;
    }
    // NOTE: Would parse JSON and check if "username" field matches.
    // Stubbed since HTTP client and JSON parsing are not implemented.
    todo!("verify_login requires HTTP client and JSON parsing")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verify_login_without_credentials_returns_false() {
        // With empty credentials, the client should fail to authenticate
        let result = verify_login("https://example.com", "", "");
        assert!(!result);
    }
}

// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden src/web_service/verify_login.h and verify_login.cpp
//!
//! Checks if username and token are valid against the web service.

use crate::web_backend::Client;

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
pub fn verify_login(host: &str, username: &str, token: &str) -> bool {
    let mut client = Client::new(host.to_string(), username.to_string(), token.to_string());
    let reply = client.get_json("/profile", false).returned_data;
    if reply.is_empty() {
        return false;
    }
    let json: serde_json::Value =
        serde_json::from_str(&reply).expect("VerifyLogin received invalid JSON");
    match json.get("username") {
        None => username.is_empty(),
        Some(value) => value.as_str() == Some(username),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Write};

    #[test]
    fn test_verify_login_without_credentials_returns_false() {
        // With empty credentials, the client should fail to authenticate
        let result = verify_login("https://example.com", "", "");
        assert!(!result);
    }

    #[test]
    fn matching_profile_username_verifies_login() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        let server = std::thread::spawn(move || {
            for (content_type, body) in [
                ("text/html", "free-jwt"),
                ("application/json", r#"{"username":"FreePlayer"}"#),
            ] {
                let (mut stream, _) = listener.accept().unwrap();
                stream
                    .set_read_timeout(Some(std::time::Duration::from_secs(2)))
                    .unwrap();
                let mut request = Vec::new();
                let mut buffer = [0_u8; 1024];
                while !request.windows(4).any(|part| part == b"\r\n\r\n") {
                    let count = stream.read(&mut buffer).unwrap();
                    assert_ne!(count, 0);
                    request.extend_from_slice(&buffer[..count]);
                }
                write!(
                    stream,
                    "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
                    body.len()
                )
                .unwrap();
            }
        });

        assert!(verify_login(
            &format!("http://{address}"),
            "FreePlayer",
            "free-token"
        ));
        server.join().unwrap();
    }
}

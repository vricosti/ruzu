// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/web_backend.h and web_backend.cpp
//!
//! Provides the HTTP client for communicating with the web service.
//! The C++ version uses cpp-httplib; this Rust port uses blocking `ureq`
//! requests to preserve the same call-site behaviour.

use std::sync::{LazyLock, Mutex};

use crate::web_result::{WebResult, WebResultCode};

// ---------------------------------------------------------------------------
// Constants (from web_backend.cpp)
// ---------------------------------------------------------------------------

/// API version string.
pub const API_VERSION: &str = "1";

/// HTTP timeout in seconds.
pub const TIMEOUT_SECONDS: u64 = 30;

#[derive(Default)]
struct JwtCache {
    username: String,
    token: String,
    jwt: String,
}

/// Upstream keeps one process-wide JWT cache, guarded by a mutex.
static JWT_CACHE: LazyLock<Mutex<JwtCache>> = LazyLock::new(|| Mutex::new(JwtCache::default()));

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
        // cpp-httplib accepts a bare host and defaults it to http on port 80;
        // ureq needs a full URL, so the same default is made explicit here.
        // A host that already carries a scheme is left alone.
        if !normalized_host.is_empty()
            && !normalized_host.starts_with("http://")
            && !normalized_host.starts_with("https://")
        {
            normalized_host = format!("http://{normalized_host}");
        }

        let jwt = {
            let cache = JWT_CACHE.lock().unwrap();
            if cache.username == username && cache.token == token {
                cache.jwt.clone()
            } else {
                String::new()
            }
        };

        Self {
            host: normalized_host,
            username,
            token,
            jwt,
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
    /// Maps to C++ `Client::Impl::GenericRequest`. The C++ side uses
    /// cpp-httplib; `ureq` is the closest Rust equivalent — blocking, like
    /// upstream, so the call sites keep the same shape.
    fn generic_request(
        &mut self,
        method: &str,
        path: &str,
        data: &str,
        allow_anonymous: bool,
        accept: &str,
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

        let jwt = self.jwt.clone();
        let mut result = self.generic_request_with_auth(method, path, data, accept, &jwt, "", "");
        if result.result_string == "401" {
            // Eden refreshes the internal JWT once and retries the request.
            self.update_jwt();
            let jwt = self.jwt.clone();
            result = self.generic_request_with_auth(method, path, data, accept, &jwt, "", "");
        }
        result
    }

    /// Generic request with an explicit authentication method, matching the
    /// second C++ `Client::Impl::GenericRequest` overload.
    fn generic_request_with_auth(
        &self,
        method: &str,
        path: &str,
        data: &str,
        accept: &str,
        jwt: &str,
        username: &str,
        token: &str,
    ) -> WebResult {
        let url = format!("{}{}", self.host, path);
        let agent = ureq::AgentBuilder::new()
            .timeout_connect(std::time::Duration::from_secs(TIMEOUT_SECONDS))
            .timeout_read(std::time::Duration::from_secs(TIMEOUT_SECONDS))
            .timeout_write(std::time::Duration::from_secs(TIMEOUT_SECONDS))
            .build();

        // Upstream sends the JWT when it has one, otherwise the
        // username/token pair, otherwise nothing (anonymous).
        let mut request = agent.request(method, &url);
        if !jwt.is_empty() {
            request = request.set("Authorization", &format!("Bearer {jwt}"));
        } else if !username.is_empty() {
            request = request.set("x-username", username);
            request = request.set("x-token", token);
        }
        request = request.set("api-version", API_VERSION);
        if method != "GET" {
            request = request.set("Content-Type", "application/json");
        }

        let response = if method == "GET" {
            request.call()
        } else {
            request.send_string(data)
        };

        match response {
            Ok(response) => {
                let Some(content_type) = response.header("content-type").map(str::to_string) else {
                    log::error!("{method} to {url} returned no content type");
                    return WebResult {
                        result_code: WebResultCode::WrongContent,
                        result_string: String::new(),
                        returned_data: String::new(),
                    };
                };
                let body = response.into_string().unwrap_or_default();
                if !accept.is_empty() && !content_type.contains(accept) {
                    log::error!("{method} to {url} returned wrong content: {content_type}");
                    return WebResult {
                        result_code: WebResultCode::WrongContent,
                        result_string: "Wrong content".to_string(),
                        returned_data: String::new(),
                    };
                }
                WebResult {
                    result_code: WebResultCode::Success,
                    result_string: String::new(),
                    returned_data: body,
                }
            }
            Err(ureq::Error::Status(code, _)) => {
                log::error!("{method} to {url} returned error status code: {code}");
                WebResult {
                    result_code: WebResultCode::HttpError,
                    result_string: code.to_string(),
                    returned_data: String::new(),
                }
            }
            Err(error) => {
                log::error!("{method} to {url} returned null: {error}");
                WebResult {
                    result_code: WebResultCode::LibError,
                    result_string: "Null response".to_string(),
                    returned_data: String::new(),
                }
            }
        }
    }

    /// Retrieve a new JWT from given username and token.
    fn update_jwt(&mut self) {
        if self.username.is_empty() || self.token.is_empty() {
            return;
        }
        let result = self.generic_request_with_auth(
            "POST",
            "/jwt/internal",
            "",
            "text/html",
            "",
            &self.username,
            &self.token,
        );
        if result.result_code != WebResultCode::Success {
            log::error!("UpdateJWT failed");
            return;
        }

        self.jwt = result.returned_data;
        let mut cache = JWT_CACHE.lock().unwrap();
        cache.username.clone_from(&self.username);
        cache.token.clone_from(&self.token);
        cache.jwt.clone_from(&self.jwt);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Write};

    fn read_http_request(stream: &mut std::net::TcpStream) -> String {
        stream
            .set_read_timeout(Some(std::time::Duration::from_secs(2)))
            .unwrap();
        let mut bytes = Vec::new();
        let mut buffer = [0_u8; 1024];
        while !bytes.windows(4).any(|window| window == b"\r\n\r\n") {
            let count = stream.read(&mut buffer).unwrap();
            if count == 0 {
                break;
            }
            bytes.extend_from_slice(&buffer[..count]);
        }
        String::from_utf8(bytes).unwrap()
    }

    fn write_http_response(stream: &mut std::net::TcpStream, content_type: &str, body: &str) {
        write!(
            stream,
            "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
            body.len()
        )
        .unwrap();
    }

    #[test]
    fn test_client_normalizes_host() {
        let client = Client::new(
            "https://example.com/".to_string(),
            String::new(),
            String::new(),
        );
        assert_eq!(client.host, "https://example.com");
    }

    /// Replaces a test that asserted the "HTTP client not implemented" stub.
    /// An unreachable host must report a transport failure rather than
    /// pretending the request succeeded — and it must not need the network to
    /// prove it, hence the reserved TEST-NET-1 address from RFC 5737.
    #[test]
    fn an_unreachable_host_reports_a_transport_failure() {
        // Bind a port, learn its number, then drop the listener: connecting
        // to it is refused immediately. An unroutable address would instead
        // sit out the full 30 s connect timeout.
        let port = {
            let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
            listener.local_addr().unwrap().port()
        };
        let mut client = Client::new(
            format!("http://127.0.0.1:{port}"),
            String::new(),
            String::new(),
        );
        let result = client.get_json("/test", true);
        assert_eq!(result.result_code, WebResultCode::LibError);
        assert!(result.returned_data.is_empty());
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

    #[test]
    fn external_jwt_uses_the_internal_jwt_as_bearer_authentication() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        let server = std::thread::spawn(move || {
            let (mut internal, _) = listener.accept().unwrap();
            let request = read_http_request(&mut internal);
            assert!(request.starts_with("POST /jwt/internal HTTP/1.1"));
            assert!(request
                .to_ascii_lowercase()
                .contains("x-username: reviewer"));
            assert!(request.to_ascii_lowercase().contains("x-token: secret"));
            write_http_response(&mut internal, "text/html", "internal-jwt");

            let (mut external, _) = listener.accept().unwrap();
            let request = read_http_request(&mut external);
            assert!(request.starts_with("POST /jwt/external/room-guid HTTP/1.1"));
            assert!(request
                .to_ascii_lowercase()
                .contains("authorization: bearer internal-jwt"));
            write_http_response(&mut external, "text/html", "external-jwt");
        });

        let mut client = Client::new(
            format!("http://{address}"),
            "reviewer".to_string(),
            "secret".to_string(),
        );
        let result = client.get_external_jwt("room-guid");
        assert_eq!(result.result_code, WebResultCode::Success);
        assert_eq!(result.returned_data, "external-jwt");
        server.join().unwrap();
    }

    #[test]
    fn an_empty_body_with_the_expected_content_type_is_successful() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        let server = std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let _ = read_http_request(&mut stream);
            write_http_response(&mut stream, "application/json", "");
        });

        let mut client = Client::new(format!("http://{address}"), String::new(), String::new());
        let result = client.get_json("/empty", true);
        assert_eq!(result.result_code, WebResultCode::Success);
        assert!(result.returned_data.is_empty());
        server.join().unwrap();
    }
}

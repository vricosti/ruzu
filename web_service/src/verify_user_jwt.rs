// SPDX-FileCopyrightText: Copyright 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/verify_user_jwt.h and verify_user_jwt.cpp
//!
//! Provides JWT-based user verification against the web service.

use crate::web_backend::Client;

// ---------------------------------------------------------------------------
// Public key fetching
// ---------------------------------------------------------------------------

/// Cached public key for JWT verification.
static mut PUBLIC_KEY: Option<String> = None;

/// Fetches the public key from the web service for JWT verification.
/// Maps to C++ `WebService::GetPublicKey`.
///
/// NOTE: The HTTP client is stubbed, so this will return an empty string.
pub fn get_public_key(host: &str) -> String {
    // NOTE: In production this would be thread-safe with a mutex.
    // Simplified for stub implementation.
    unsafe {
        if let Some(ref key) = PUBLIC_KEY {
            return key.clone();
        }
    }
    let mut client = Client::new(host.to_string(), String::new(), String::new());
    let key = client.get_plain("/jwt/external/key.pem", true).returned_data;
    if key.is_empty() {
        log::error!("Could not fetch external JWT public key, verification may fail");
    } else {
        log::info!("Fetched external JWT public key (size={})", key.len());
    }
    unsafe {
        PUBLIC_KEY = Some(key.clone());
    }
    key
}

// ---------------------------------------------------------------------------
// VerifyUserJWT
// ---------------------------------------------------------------------------

/// JWT-based user verification backend.
/// Maps to C++ `WebService::VerifyUserJWT`.
///
/// This struct implements the `network::verify_user::Backend` trait
/// conceptually, but the actual trait impl requires the `network` crate as a
/// dependency. The method signature is preserved for structural parity.
pub struct VerifyUserJwt {
    pub_key: String,
}

/// User data returned from JWT verification.
/// Re-uses the same field layout as `network::verify_user::UserData`.
#[derive(Clone, Debug, Default)]
pub struct UserData {
    pub username: String,
    pub display_name: String,
    pub avatar_url: String,
    pub moderator: bool,
}

impl VerifyUserJwt {
    pub fn new(host: &str) -> Self {
        Self {
            pub_key: get_public_key(host),
        }
    }

    /// Verifies the given token and loads user data from the JWT claims.
    ///
    /// NOTE: JWT decoding/verification is not implemented (requires a JWT
    /// library). This is stubbed to return an empty `UserData`.
    pub fn load_user_data(&self, verify_uid: &str, token: &str) -> UserData {
        let _audience = format!("external-{}", verify_uid);
        // NOTE: Would decode JWT with RS256, verify issuer="citra-core",
        // audience, iat, jti, then extract claims.
        // Stubbed because JWT library is not available.
        if self.pub_key.is_empty() || token.is_empty() {
            log::info!("Verification failed: missing public key or token");
            return UserData::default();
        }
        todo!("VerifyUserJwt::load_user_data requires JWT library")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verify_user_jwt_empty_key() {
        let verifier = VerifyUserJwt {
            pub_key: String::new(),
        };
        let data = verifier.load_user_data("uid", "");
        assert!(data.username.is_empty());
    }
}

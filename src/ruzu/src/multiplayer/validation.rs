// Rust counterpart of `yuzu/multiplayer/validation.h`.
//
// GTK does not have Qt's `QValidator` API, so the same accepted value sets are
// exposed as predicates used when a dialog is submitted.

use std::net::{Ipv4Addr, Ipv6Addr};
use std::str::FromStr;

pub const DEFAULT_ROOM_PORT: u16 = 24872;

/// Upstream nickname validator: 4-20 ASCII characters drawn from
/// alphanumerics, space, `_`, `.`, and `-`.
pub fn is_valid_nickname(nickname: &str) -> bool {
    let length = nickname.chars().count();
    (4..=20).contains(&length)
        && nickname.chars().all(|character| {
            character.is_ascii_alphanumeric() || matches!(character, ' ' | '-' | '_' | '.')
        })
}

/// Upstream `QIntValidator(0, UINT16_MAX)`, with an empty field restoring the
/// setting default when the dialog is submitted.
pub fn parse_port(text: &str) -> Option<u16> {
    if text.is_empty() {
        return Some(DEFAULT_ROOM_PORT);
    }
    text.parse::<u16>().ok()
}

/// Upstream accepts IPv4, IPv6 (including a scope suffix), or a dotted DNS
/// hostname whose final label is alphabetic and at least two characters.
pub fn is_valid_address(address: &str) -> bool {
    if Ipv4Addr::from_str(address).is_ok() {
        return true;
    }

    let ipv6 = match address.split_once('%') {
        Some((_host, "")) => return false,
        Some((host, _scope)) => host,
        None => address,
    };
    if Ipv6Addr::from_str(ipv6).is_ok() {
        return true;
    }

    let labels: Vec<&str> = address.split('.').collect();
    if labels.len() < 2
        || labels.last().is_none_or(|label| {
            label.len() < 2 || !label.bytes().all(|byte| byte.is_ascii_alphabetic())
        })
    {
        return false;
    }

    labels.iter().all(|label| {
        !label.is_empty()
            && label
                .as_bytes()
                .first()
                .is_some_and(u8::is_ascii_alphanumeric)
            && label
                .as_bytes()
                .last()
                .is_some_and(u8::is_ascii_alphanumeric)
            && label
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nickname_matches_the_upstream_character_set_and_bounds() {
        assert!(!is_valid_nickname("abc"));
        assert!(is_valid_nickname("abcd"));
        assert!(is_valid_nickname("Player One"));
        assert!(is_valid_nickname("a_very-long.name123"));
        assert!(!is_valid_nickname("this-nickname-is-far-too-long"));
        assert!(!is_valid_nickname("bad/name"));
    }

    #[test]
    fn port_matches_qintvalidator_bounds() {
        assert_eq!(parse_port(""), Some(DEFAULT_ROOM_PORT));
        assert_eq!(parse_port("0"), Some(0));
        assert_eq!(parse_port("65535"), Some(65535));
        assert_eq!(parse_port("70000"), None);
        assert_eq!(parse_port("abc"), None);
    }

    #[test]
    fn address_matches_ip_and_hostname_forms() {
        assert!(is_valid_address("127.0.0.1"));
        assert!(is_valid_address("2001:db8::1"));
        assert!(is_valid_address("fe80::1%eth0"));
        assert!(!is_valid_address("fe80::1%"));
        assert!(is_valid_address("room.example.org"));
        assert!(!is_valid_address(""));
        assert!(!is_valid_address("not a host"));
        assert!(!is_valid_address("localhost"));
        assert!(!is_valid_address("256.0.0.1"));
    }
}

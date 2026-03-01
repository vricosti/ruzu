// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Ticket parsing for extracting title keys from NSP packages.
//!
//! A ticket (`.tik` file) contains the encrypted title key and the rights ID
//! that identifies which NCA it decrypts. The title key in the ticket body
//! is encrypted with a titlekek derived from the master key at the ticket's
//! crypto revision.

use crate::key_manager::{Key128, KeyManager};
use thiserror::Error;

/// Minimum ticket size to parse the fields we need.
const MIN_TICKET_SIZE: usize = 0x2C0;

/// Offset of the title key block in the ticket body.
const TITLE_KEY_BLOCK_OFFSET: usize = 0x180;

/// Offset of the title key type field.
const TITLE_KEY_TYPE_OFFSET: usize = 0x1DC;

/// Offset of the master key revision field.
const KEY_REVISION_OFFSET: usize = 0x1F0;

/// Offset of the rights ID (16 bytes).
const RIGHTS_ID_OFFSET: usize = 0x2A0;

/// Errors from ticket parsing.
#[derive(Debug, Error)]
pub enum TicketError {
    #[error("ticket too small: need at least {MIN_TICKET_SIZE} bytes, got {0}")]
    TooSmall(usize),
}

/// Title key type in ticket.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TitleKeyType {
    /// Title key is stored as a 16-byte common key (plaintext in the ticket).
    Common = 0,
    /// Title key is encrypted with a personalized key (not supported).
    Personalized = 1,
}

/// Parsed ticket data.
#[derive(Debug, Clone)]
pub struct Ticket {
    /// Rights ID identifying which NCA this ticket decrypts.
    pub rights_id: [u8; 16],
    /// Rights ID as a hex string.
    pub rights_id_hex: String,
    /// Title key type.
    pub title_key_type: TitleKeyType,
    /// Raw title key block (may be encrypted).
    pub title_key_block: Key128,
    /// Master key revision for titlekek lookup.
    pub key_revision: u8,
}

impl Ticket {
    /// Parse a ticket from raw bytes.
    pub fn parse(data: &[u8]) -> Result<Self, TicketError> {
        if data.len() < MIN_TICKET_SIZE {
            return Err(TicketError::TooSmall(data.len()));
        }

        // Title key block (first 16 bytes at offset 0x180)
        let mut title_key_block = [0u8; 16];
        title_key_block.copy_from_slice(&data[TITLE_KEY_BLOCK_OFFSET..TITLE_KEY_BLOCK_OFFSET + 16]);

        // Title key type
        let title_key_type = match data[TITLE_KEY_TYPE_OFFSET] {
            0 => TitleKeyType::Common,
            _ => TitleKeyType::Personalized,
        };

        // Key revision
        let key_revision = data[KEY_REVISION_OFFSET];

        // Rights ID
        let mut rights_id = [0u8; 16];
        rights_id.copy_from_slice(&data[RIGHTS_ID_OFFSET..RIGHTS_ID_OFFSET + 16]);
        let rights_id_hex = hex::encode(rights_id);

        Ok(Self {
            rights_id,
            rights_id_hex,
            title_key_type,
            title_key_block,
            key_revision,
        })
    }

    /// Register the title key (encrypted) in the key manager.
    ///
    /// The title key block from the ticket is stored as-is (still encrypted
    /// with the titlekek). Decryption happens later at NCA open time in
    /// `get_body_key()`, matching zuyu's architecture where
    /// `content_archive.cpp` decrypts with `AESCipher(titlekek, ECB)`.
    ///
    /// If a title key is already present in the key manager (e.g., from
    /// title.keys), it is kept since both sources store the same encrypted key.
    pub fn register_title_key(&self, keys: &mut KeyManager) -> Result<Key128, TicketError> {
        // If title.keys already has this key, keep it
        if let Some(existing) = keys.title_key(&self.rights_id_hex) {
            log::info!(
                "Title key for rights_id={} already present (from title.keys), keeping it",
                self.rights_id_hex
            );
            return Ok(existing);
        }

        // Store the raw title key block (encrypted with titlekek).
        // For common tickets, this is the titlekek-encrypted key.
        // For personalized tickets, this is the raw key block.
        let title_key = self.title_key_block;

        if self.title_key_type == TitleKeyType::Personalized {
            log::warn!(
                "Personalized ticket for rights_id={}, using raw key block",
                self.rights_id_hex
            );
        }

        keys.add_title_key(&self.rights_id_hex, title_key);
        log::info!(
            "Registered title key (encrypted) for rights_id={}",
            self.rights_id_hex
        );

        Ok(title_key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_ticket(key_type: u8, revision: u8, rights_id: &[u8; 16]) -> Vec<u8> {
        let mut data = vec![0u8; MIN_TICKET_SIZE];
        // Title key block at 0x180
        data[TITLE_KEY_BLOCK_OFFSET..TITLE_KEY_BLOCK_OFFSET + 16]
            .copy_from_slice(&[0xAA; 16]);
        // Title key type
        data[TITLE_KEY_TYPE_OFFSET] = key_type;
        // Key revision
        data[KEY_REVISION_OFFSET] = revision;
        // Rights ID
        data[RIGHTS_ID_OFFSET..RIGHTS_ID_OFFSET + 16].copy_from_slice(rights_id);
        data
    }

    #[test]
    fn test_parse_ticket() {
        let rights_id = [
            0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x01,
        ];
        let data = make_ticket(0, 0, &rights_id);
        let ticket = Ticket::parse(&data).unwrap();

        assert_eq!(ticket.rights_id, rights_id);
        assert_eq!(ticket.title_key_type, TitleKeyType::Common);
        assert_eq!(ticket.key_revision, 0);
        assert_eq!(ticket.title_key_block, [0xAA; 16]);
        assert_eq!(ticket.rights_id_hex, "01000000000000000000000000000001");
    }

    #[test]
    fn test_ticket_too_small() {
        let data = vec![0u8; 16];
        assert!(matches!(Ticket::parse(&data), Err(TicketError::TooSmall(_))));
    }

    #[test]
    fn test_personalized_ticket() {
        let rights_id = [0x02; 16];
        let data = make_ticket(1, 0, &rights_id);
        let ticket = Ticket::parse(&data).unwrap();
        assert_eq!(ticket.title_key_type, TitleKeyType::Personalized);
    }
}

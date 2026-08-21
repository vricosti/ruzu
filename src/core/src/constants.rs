//! Port of zuyu/src/core/constants.h and zuyu/src/core/constants.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! System-wide constants used by multiple components.
//! This consolidates constants to prevent duplication across the codebase.

/// ACC Service - JPEG used as the user icon when no custom image exists.
///
/// Upstream embeds its 256x256 emulator profile image in this constant. Ruzu
/// keeps the same ownership and dimensions, with the Ruzu profile artwork.
pub const ACCOUNT_BACKUP_JPEG: [u8; 20_358] = *include_bytes!("../assets/ruzu_profile.jpg");

#[cfg(test)]
mod tests {
    use super::ACCOUNT_BACKUP_JPEG;

    #[test]
    fn account_backup_jpeg_matches_upstream_dimensions() {
        let image =
            image::load_from_memory_with_format(&ACCOUNT_BACKUP_JPEG, image::ImageFormat::Jpeg)
                .expect("account backup must be a valid JPEG");

        assert_eq!((image.width(), image.height()), (256, 256));
    }
}

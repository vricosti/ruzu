// Port of `yuzu/multiplayer/message.h` and `yuzu/multiplayer/message.cpp`
// (`NetworkMessage::ErrorManager`).
//
// Upstream reports every multiplayer failure through a modal `QMessageBox`,
// not through an inline status line, so a failed join is impossible to miss.
// The strings below are copied verbatim from `message.cpp`; only the product
// name changes, since the dialogs name the running emulator. Messages used
// only by Eden's host-room dialog are omitted until Ruzu has that dialog.
//
// `WarnGameRunning`, `WarnCloseRoom` and `WarnDisconnect` are not ported yet:
// upstream's `WarnMessage` blocks on `QMessageBox::warning` and returns the
// answer, which GTK cannot do — its dialogs are asynchronous — and no Ruzu
// call site needs them today. They belong in this file when one does.

use network::room_member::RoomMemberError;

/// Upstream `NetworkMessage::ConnectionError`: a wrapper around the message
/// shown to the user.
pub struct ConnectionError(&'static str);

impl ConnectionError {
    /// Upstream `ConnectionError::GetString`.
    pub fn get_string(&self) -> &'static str {
        self.0
    }
}

/// Upstream `NetworkMessage::ErrorManager`.
pub struct ErrorManager;

impl ErrorManager {
    pub const USERNAME_NOT_VALID: ConnectionError =
        ConnectionError("Username is not valid. Must be 4 to 20 alphanumeric characters.");
    pub const USERNAME_NOT_VALID_SERVER: ConnectionError =
        ConnectionError("Username is already in use or not valid. Please choose another.");
    pub const IP_ADDRESS_NOT_VALID: ConnectionError =
        ConnectionError("IP is not a valid IPv4 address.");
    pub const PORT_NOT_VALID: ConnectionError =
        ConnectionError("Port must be a number between 0 to 65535.");
    pub const UNABLE_TO_CONNECT: ConnectionError = ConnectionError(
        "Unable to connect to the host. Verify that the connection settings are correct. If you \
         still cannot connect, contact the room host and verify that the host is properly \
         configured with the external port forwarded.",
    );
    pub const ROOM_IS_FULL: ConnectionError =
        ConnectionError("Unable to connect to the room because it is already full.");
    pub const HOST_BANNED: ConnectionError = ConnectionError(
        "The host of the room has banned you. Speak with the host to unban you or try a \
         different room.",
    );
    pub const WRONG_VERSION: ConnectionError = ConnectionError(
        "Version mismatch! Please update to the latest version of Ruzu. If the problem \
         persists, contact the room host and ask them to update the server.",
    );
    pub const WRONG_PASSWORD: ConnectionError = ConnectionError("Incorrect password.");
    pub const LOST_CONNECTION: ConnectionError =
        ConnectionError("Connection to room lost. Try to reconnect.");
    pub const HOST_KICKED: ConnectionError =
        ConnectionError("You have been kicked by the room host.");
    pub const IP_COLLISION: ConnectionError =
        ConnectionError("IP address is already in use. Please choose another.");
    pub const PERMISSION_DENIED: ConnectionError =
        ConnectionError("You do not have enough permission to perform this action.");
    pub const NO_SUCH_USER: ConnectionError = ConnectionError(
        "The user you are trying to kick/ban could not be found.\nThey may have left the room.",
    );
    pub const NO_INTERFACE_SELECTED: ConnectionError = ConnectionError(
        "No valid network interface is selected.\nPlease go to Configure -> System -> Network \
         and make a selection.",
    );

    /// Upstream `MultiplayerState::OnNetworkError` mapping.
    pub fn for_room_member_error(error: RoomMemberError) -> &'static ConnectionError {
        match error {
            RoomMemberError::LostConnection => &Self::LOST_CONNECTION,
            RoomMemberError::HostKicked => &Self::HOST_KICKED,
            RoomMemberError::CouldNotConnect | RoomMemberError::UnknownError => {
                &Self::UNABLE_TO_CONNECT
            }
            RoomMemberError::NameCollision => &Self::USERNAME_NOT_VALID_SERVER,
            RoomMemberError::IpCollision => &Self::IP_COLLISION,
            RoomMemberError::RoomIsFull => &Self::ROOM_IS_FULL,
            RoomMemberError::WrongPassword => &Self::WRONG_PASSWORD,
            RoomMemberError::WrongVersion => &Self::WRONG_VERSION,
            RoomMemberError::HostBanned => &Self::HOST_BANNED,
            RoomMemberError::PermissionDenied => &Self::PERMISSION_DENIED,
            RoomMemberError::NoSuchUser => &Self::NO_SUCH_USER,
        }
    }

    /// GTK counterpart of upstream `ErrorManager::ShowError`, with the same
    /// `Error` title and a transient parent when the active dialog is known.
    pub fn show_error(parent: Option<&gtk::Window>, error: &ConnectionError) {
        log::error!("Multiplayer error: {}", error.get_string());
        crate::gtk_compat::show_error(parent, "Error", error.get_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn room_member_errors_use_edens_message_mapping() {
        for (error, expected) in [
            (
                RoomMemberError::LostConnection,
                &ErrorManager::LOST_CONNECTION,
            ),
            (RoomMemberError::HostKicked, &ErrorManager::HOST_KICKED),
            (
                RoomMemberError::CouldNotConnect,
                &ErrorManager::UNABLE_TO_CONNECT,
            ),
            (
                RoomMemberError::UnknownError,
                &ErrorManager::UNABLE_TO_CONNECT,
            ),
            (
                RoomMemberError::NameCollision,
                &ErrorManager::USERNAME_NOT_VALID_SERVER,
            ),
            (RoomMemberError::IpCollision, &ErrorManager::IP_COLLISION),
            (RoomMemberError::RoomIsFull, &ErrorManager::ROOM_IS_FULL),
            (
                RoomMemberError::WrongPassword,
                &ErrorManager::WRONG_PASSWORD,
            ),
            (RoomMemberError::WrongVersion, &ErrorManager::WRONG_VERSION),
            (RoomMemberError::HostBanned, &ErrorManager::HOST_BANNED),
            (
                RoomMemberError::PermissionDenied,
                &ErrorManager::PERMISSION_DENIED,
            ),
            (RoomMemberError::NoSuchUser, &ErrorManager::NO_SUCH_USER),
        ] {
            assert_eq!(
                ErrorManager::for_room_member_error(error).get_string(),
                expected.get_string(),
                "wrong message for {error:?}"
            );
        }
    }

    #[test]
    fn product_specific_errors_name_ruzu() {
        assert!(ErrorManager::WRONG_VERSION.get_string().contains("Ruzu"));
        assert!(!ErrorManager::WRONG_VERSION.get_string().contains("Reden"));
    }
}

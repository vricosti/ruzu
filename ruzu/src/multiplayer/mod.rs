// Port of the `yuzu/multiplayer/` folder.
//
// Only the dialogs whose network dependency is implemented live here.
// `RoomMember` provides the ENet client transport and
// `AnnounceMultiplayerSession` provides public lobby discovery. Hosting still
// depends on the unfinished `Room` server, so `host_room` and its moderation
// dialog remain absent.

pub mod client_room;
pub mod direct_connect;
pub mod lobby;
pub mod message;
pub mod validation;

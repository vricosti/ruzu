// SPDX-FileCopyrightText: 2026 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Opaque Host1x bridge used by `core` owners that must talk to the
//! frontend-provided Host1x implementation without depending on `video_core`.

use std::any::Any;

pub trait Host1xCoreInterface: Any + Send + Sync {
    fn as_any(&self) -> &(dyn Any + Send + Sync);
    fn get_host_syncpoint_value(&self, id: u32) -> u32;
    fn wait_host(&self, id: u32, expected_value: u32);
    fn register_host_action(
        &self,
        id: u32,
        expected_value: u32,
        action: Box<dyn FnOnce() + Send>,
    ) -> Option<u64>;
    fn deregister_host_action(&self, id: u32, handle: u64);
}

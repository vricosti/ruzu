// SPDX-License-Identifier: GPL-3.0-or-later
//
// GTK counterpart of upstream `yuzu/util/controller_navigation.{h,cpp}`.

use std::collections::VecDeque;
use std::sync::Arc;

use common::input::{ButtonStatus, StickStatus};
use common::settings_input::{native_analog, native_button};
use hid_core::frontend::emulated_controller::{ControllerTriggerType, ControllerUpdateCallback};
use hid_core::hid_core::{EmulatedControllerHandle, HIDCore};
use hid_core::hid_types::{NpadIdType, NpadStyleIndex};
use parking_lot::Mutex;

/// Keyboard-equivalent actions emitted by upstream `ControllerNavigation`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NavigationKey {
    Enter,
    Escape,
    Down,
    Left,
    Right,
    Up,
}

struct NavigationState {
    button_values: Vec<ButtonStatus>,
    stick_values: Vec<StickStatus>,
    pending_triggers: VecDeque<ControllerTriggerType>,
    pending_keys: VecDeque<NavigationKey>,
}

impl Default for NavigationState {
    fn default() -> Self {
        Self {
            button_values: vec![ButtonStatus::default(); native_button::NUM_BUTTONS],
            stick_values: vec![StickStatus::default(); native_analog::NUM_ANALOGS],
            pending_triggers: VecDeque::new(),
            pending_keys: VecDeque::new(),
        }
    }
}

/// Port of upstream `ControllerNavigation`.
///
/// GTK objects are main-thread-only, while HID callbacks are `Send + Sync`.
/// The callback therefore queues upstream's keyboard-equivalent action and the
/// owning widget drains it from the GTK main loop.
pub struct ControllerNavigation {
    state: Arc<Mutex<NavigationState>>,
    player_1_controller: EmulatedControllerHandle,
    handheld_controller: EmulatedControllerHandle,
    player_1_callback_key: i32,
    handheld_callback_key: i32,
}

impl ControllerNavigation {
    pub fn new(hid_core: &Arc<Mutex<HIDCore>>) -> Self {
        let (player_1_controller, handheld_controller) = {
            let hid_core = hid_core.lock();
            (
                hid_core.get_emulated_controller(NpadIdType::Player1),
                hid_core.get_emulated_controller(NpadIdType::Handheld),
            )
        };
        let state = Arc::new(Mutex::new(NavigationState::default()));

        let callback_state = Arc::clone(&state);
        let on_change: Arc<dyn Fn(ControllerTriggerType) + Send + Sync> =
            Arc::new(move |trigger_type| {
                callback_state
                    .lock()
                    .pending_triggers
                    .push_back(trigger_type);
            });

        let player_1_callback_key =
            player_1_controller
                .lock()
                .set_callback(ControllerUpdateCallback {
                    on_change: Arc::clone(&on_change),
                    is_npad_service: false,
                });
        let handheld_callback_key =
            handheld_controller
                .lock()
                .set_callback(ControllerUpdateCallback {
                    on_change,
                    is_npad_service: false,
                });

        Self {
            state,
            player_1_controller,
            handheld_controller,
            player_1_callback_key,
            handheld_callback_key,
        }
    }

    /// Drain keyboard-equivalent actions on the GTK main thread.
    pub fn take_pending_keys(&self) -> Vec<NavigationKey> {
        let triggers: Vec<_> = self.state.lock().pending_triggers.drain(..).collect();
        for trigger_type in triggers {
            controller_update_event(
                &self.state,
                &self.player_1_controller,
                &self.handheld_controller,
                trigger_type,
            );
        }
        self.state.lock().pending_keys.drain(..).collect()
    }

    /// Discard events received while the list is hidden or inactive.
    pub fn discard_pending_keys(&self) {
        let mut state = self.state.lock();
        state.pending_triggers.clear();
        state.pending_keys.clear();
    }

    /// Upstream `ControllerNavigation::UnloadController`.
    pub fn unload_controller(&mut self) {
        if self.player_1_callback_key >= 0 {
            self.player_1_controller
                .lock()
                .delete_callback(self.player_1_callback_key);
            self.player_1_callback_key = -1;
        }
        if self.handheld_callback_key >= 0 {
            self.handheld_controller
                .lock()
                .delete_callback(self.handheld_callback_key);
            self.handheld_callback_key = -1;
        }
    }
}

impl Drop for ControllerNavigation {
    fn drop(&mut self) {
        self.unload_controller();
    }
}

fn controller_update_event(
    state: &Mutex<NavigationState>,
    player_1_controller: &EmulatedControllerHandle,
    handheld_controller: &EmulatedControllerHandle,
    trigger_type: ControllerTriggerType,
) {
    let enabled = *common::settings::values().controller_navigation.get_value();
    if !enabled {
        return;
    }

    match trigger_type {
        ControllerTriggerType::Button => {
            controller_update_button(state, player_1_controller, handheld_controller)
        }
        ControllerTriggerType::Stick => {
            controller_update_stick(state, player_1_controller, handheld_controller)
        }
        _ => {}
    }
}

fn controller_update_button(
    state: &Mutex<NavigationState>,
    player_1_controller: &EmulatedControllerHandle,
    handheld_controller: &EmulatedControllerHandle,
) {
    let (controller_type, player_1_buttons) = {
        let controller = player_1_controller.lock();
        (
            controller.get_npad_style_index(false),
            controller.get_buttons_values(),
        )
    };
    let handheld_buttons = handheld_controller.lock().get_buttons_values();
    let mut state = state.lock();

    for index in 0..state.button_values.len() {
        let button = player_1_buttons[index].value || handheld_buttons[index].value;
        state.button_values[index].locked = button == state.button_values[index].value;
        state.button_values[index].value = button;
    }

    match controller_type {
        NpadStyleIndex::Fullkey
        | NpadStyleIndex::JoyconDual
        | NpadStyleIndex::Handheld
        | NpadStyleIndex::GameCube => {
            trigger_button(&mut state, native_button::Values::A, NavigationKey::Enter);
            trigger_button(&mut state, native_button::Values::B, NavigationKey::Escape);
            trigger_button(
                &mut state,
                native_button::Values::DDown,
                NavigationKey::Down,
            );
            trigger_button(
                &mut state,
                native_button::Values::DLeft,
                NavigationKey::Left,
            );
            trigger_button(
                &mut state,
                native_button::Values::DRight,
                NavigationKey::Right,
            );
            trigger_button(&mut state, native_button::Values::DUp, NavigationKey::Up);
        }
        NpadStyleIndex::JoyconLeft => {
            trigger_button(
                &mut state,
                native_button::Values::DDown,
                NavigationKey::Enter,
            );
            trigger_button(
                &mut state,
                native_button::Values::DLeft,
                NavigationKey::Escape,
            );
        }
        NpadStyleIndex::JoyconRight => {
            trigger_button(&mut state, native_button::Values::X, NavigationKey::Enter);
            trigger_button(&mut state, native_button::Values::A, NavigationKey::Escape);
        }
        _ => {}
    }
}

fn trigger_button(
    state: &mut NavigationState,
    native_button: native_button::Values,
    key: NavigationKey,
) {
    let button = state.button_values[native_button as usize];
    if button.value && !button.locked {
        state.pending_keys.push_back(key);
    }
}

fn controller_update_stick(
    state: &Mutex<NavigationState>,
    player_1_controller: &EmulatedControllerHandle,
    _handheld_controller: &EmulatedControllerHandle,
) {
    let (controller_type, player_1_sticks) = {
        let controller = player_1_controller.lock();
        (
            controller.get_npad_style_index(false),
            controller.get_sticks_values(),
        )
    };

    // This deliberately follows upstream: its `handheld_sticks` reference is
    // currently obtained from `player1_controller`, not `handheld_controller`.
    let handheld_sticks = player_1_controller.lock().get_sticks_values();
    let mut state = state.lock();
    let mut update = false;

    for index in 0..state.stick_values.len() {
        let stick = StickStatus {
            left: player_1_sticks[index].left || handheld_sticks[index].left,
            right: player_1_sticks[index].right || handheld_sticks[index].right,
            up: player_1_sticks[index].up || handheld_sticks[index].up,
            down: player_1_sticks[index].down || handheld_sticks[index].down,
            ..StickStatus::default()
        };
        if stick.down != state.stick_values[index].down
            || stick.left != state.stick_values[index].left
            || stick.right != state.stick_values[index].right
            || stick.up != state.stick_values[index].up
        {
            update = true;
        }
        state.stick_values[index] = stick;
    }

    if !update {
        return;
    }
    if let Some(key) = stick_navigation_key(controller_type, &state.stick_values) {
        state.pending_keys.push_back(key);
    }
}

fn stick_navigation_key(
    controller_type: NpadStyleIndex,
    stick_values: &[StickStatus],
) -> Option<NavigationKey> {
    match controller_type {
        NpadStyleIndex::Fullkey
        | NpadStyleIndex::JoyconDual
        | NpadStyleIndex::Handheld
        | NpadStyleIndex::GameCube => {
            let stick = stick_values[native_analog::Values::LStick as usize];
            if stick.down {
                Some(NavigationKey::Down)
            } else if stick.left {
                Some(NavigationKey::Left)
            } else if stick.right {
                Some(NavigationKey::Right)
            } else if stick.up {
                Some(NavigationKey::Up)
            } else {
                None
            }
        }
        NpadStyleIndex::JoyconLeft => {
            let stick = stick_values[native_analog::Values::LStick as usize];
            if stick.left {
                Some(NavigationKey::Down)
            } else if stick.up {
                Some(NavigationKey::Left)
            } else if stick.down {
                Some(NavigationKey::Right)
            } else if stick.right {
                Some(NavigationKey::Up)
            } else {
                None
            }
        }
        NpadStyleIndex::JoyconRight => {
            let stick = stick_values[native_analog::Values::RStick as usize];
            if stick.right {
                Some(NavigationKey::Down)
            } else if stick.down {
                Some(NavigationKey::Left)
            } else if stick.up {
                Some(NavigationKey::Right)
            } else if stick.left {
                Some(NavigationKey::Up)
            } else {
                None
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fullkey_left_stick_matches_upstream_key_priority() {
        let mut sticks = vec![StickStatus::default(); native_analog::NUM_ANALOGS];
        sticks[native_analog::Values::LStick as usize].down = true;
        sticks[native_analog::Values::LStick as usize].right = true;

        assert_eq!(
            stick_navigation_key(NpadStyleIndex::Fullkey, &sticks),
            Some(NavigationKey::Down)
        );
    }

    #[test]
    fn sideways_joycons_rotate_navigation_like_upstream() {
        let mut sticks = vec![StickStatus::default(); native_analog::NUM_ANALOGS];
        sticks[native_analog::Values::LStick as usize].left = true;
        assert_eq!(
            stick_navigation_key(NpadStyleIndex::JoyconLeft, &sticks),
            Some(NavigationKey::Down)
        );

        sticks.fill(StickStatus::default());
        sticks[native_analog::Values::RStick as usize].right = true;
        assert_eq!(
            stick_navigation_key(NpadStyleIndex::JoyconRight, &sticks),
            Some(NavigationKey::Down)
        );
    }

    #[test]
    fn button_front_triggers_only_once() {
        let mut state = NavigationState::default();
        let index = native_button::Values::A as usize;

        state.button_values[index].value = true;
        state.button_values[index].locked = false;
        trigger_button(&mut state, native_button::Values::A, NavigationKey::Enter);
        state.button_values[index].locked = true;
        trigger_button(&mut state, native_button::Values::A, NavigationKey::Enter);

        assert_eq!(
            state.pending_keys.into_iter().collect::<Vec<_>>(),
            vec![NavigationKey::Enter]
        );
    }

    #[test]
    fn hid_callback_only_queues_the_trigger() {
        let state = Arc::new(Mutex::new(NavigationState::default()));
        let callback_state = Arc::clone(&state);
        let callback = move |trigger_type| {
            callback_state
                .lock()
                .pending_triggers
                .push_back(trigger_type);
        };

        callback(ControllerTriggerType::Button);

        let state = state.lock();
        assert_eq!(
            state.pending_triggers.front(),
            Some(&ControllerTriggerType::Button)
        );
        assert!(state.pending_keys.is_empty());
    }
}

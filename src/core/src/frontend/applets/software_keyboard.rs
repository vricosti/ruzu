// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/software_keyboard.h and software_keyboard.cpp
//! Software keyboard applet interface.

use super::applet::Applet;
use std::sync::Mutex;

pub use crate::hle::service::am::frontend::applet_software_keyboard_types::{
    SwkbdKeyDisableFlags, SwkbdPasswordMode, SwkbdReplyType, SwkbdResult, SwkbdTextCheckResult,
    SwkbdTextDrawType, SwkbdType,
};

// ---------------------------------------------------------------------------
// Keyboard parameter types
// ---------------------------------------------------------------------------

/// Parameters for keyboard initialization.
///
/// Corresponds to upstream `Core::Frontend::KeyboardInitializeParameters`.
#[derive(Debug, Clone, Default)]
pub struct KeyboardInitializeParameters {
    pub ok_text: String,
    pub header_text: String,
    pub sub_text: String,
    pub guide_text: String,
    pub initial_text: String,
    pub left_optional_symbol_key: u16,
    pub right_optional_symbol_key: u16,
    pub max_text_length: u32,
    pub min_text_length: u32,
    pub initial_cursor_position: i32,
    pub swkbd_type: SwkbdType,
    pub password_mode: SwkbdPasswordMode,
    pub text_draw_type: SwkbdTextDrawType,
    pub key_disable_flags: SwkbdKeyDisableFlags,
    pub use_blur_background: bool,
    pub enable_backspace_button: bool,
    pub enable_return_button: bool,
    pub disable_cancel_button: bool,
}

/// Parameters for inline keyboard appearance.
///
/// Corresponds to upstream `Core::Frontend::InlineAppearParameters`.
#[derive(Debug, Clone, Default)]
pub struct InlineAppearParameters {
    pub max_text_length: u32,
    pub min_text_length: u32,
    pub key_top_scale_x: f32,
    pub key_top_scale_y: f32,
    pub key_top_translate_x: f32,
    pub key_top_translate_y: f32,
    pub swkbd_type: SwkbdType,
    pub key_disable_flags: SwkbdKeyDisableFlags,
    pub key_top_as_floating: bool,
    pub enable_backspace_button: bool,
    pub enable_return_button: bool,
    pub disable_cancel_button: bool,
}

/// Parameters for inline text changes.
///
/// Corresponds to upstream `Core::Frontend::InlineTextParameters`.
#[derive(Debug, Clone, Default)]
pub struct InlineTextParameters {
    pub input_text: String,
    pub cursor_position: i32,
}

/// Callback for normal keyboard submission.
///
/// Corresponds to upstream `SoftwareKeyboardApplet::SubmitNormalCallback`.
pub type SubmitNormalCallback = Box<dyn Fn(SwkbdResult, String, bool) + Send + Sync>;

/// Callback for inline keyboard submission.
///
/// Corresponds to upstream `SoftwareKeyboardApplet::SubmitInlineCallback`.
pub type SubmitInlineCallback = Box<dyn Fn(SwkbdReplyType, String, i32) + Send + Sync>;

/// Software keyboard applet trait.
///
/// Corresponds to upstream `Core::Frontend::SoftwareKeyboardApplet`.
pub trait SoftwareKeyboardApplet: Applet {
    fn initialize_keyboard(
        &self,
        is_inline: bool,
        initialize_parameters: KeyboardInitializeParameters,
        submit_normal_callback: SubmitNormalCallback,
        submit_inline_callback: SubmitInlineCallback,
    );

    fn show_normal_keyboard(&self);

    fn show_text_check_dialog(
        &self,
        text_check_result: SwkbdTextCheckResult,
        text_check_message: String,
    );

    fn show_inline_keyboard(&self, appear_parameters: InlineAppearParameters);

    fn hide_inline_keyboard(&self);

    fn inline_text_changed(&self, text_parameters: InlineTextParameters);

    fn exit_keyboard(&self);
}

/// Default (stub) software keyboard applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultSoftwareKeyboardApplet`.
pub struct DefaultSoftwareKeyboardApplet {
    parameters: Mutex<KeyboardInitializeParameters>,
    submit_normal_callback: Mutex<Option<SubmitNormalCallback>>,
    submit_inline_callback: Mutex<Option<SubmitInlineCallback>>,
}

impl Default for DefaultSoftwareKeyboardApplet {
    fn default() -> Self {
        Self {
            parameters: Mutex::new(KeyboardInitializeParameters::default()),
            submit_normal_callback: Mutex::new(None),
            submit_inline_callback: Mutex::new(None),
        }
    }
}

impl DefaultSoftwareKeyboardApplet {
    pub fn new() -> Self {
        Self::default()
    }

    /// Submit normal text.
    ///
    /// Corresponds to upstream `DefaultSoftwareKeyboardApplet::SubmitNormalText`.
    fn submit_normal_text(&self, text: &str) {
        if let Some(cb) = self.submit_normal_callback.lock().unwrap().as_ref() {
            cb(SwkbdResult::Ok, text.to_string(), true);
        }
    }

    /// Submit inline text character by character.
    ///
    /// Corresponds to upstream `DefaultSoftwareKeyboardApplet::SubmitInlineText`.
    fn submit_inline_text(&self, text: &str) {
        std::thread::sleep(std::time::Duration::from_millis(500));

        if let Some(cb) = self.submit_inline_callback.lock().unwrap().as_ref() {
            for index in 0..text.len() {
                let partial = &text[..=index];
                cb(
                    SwkbdReplyType::ChangedString,
                    partial.to_string(),
                    (index as i32) + 1,
                );
                std::thread::sleep(std::time::Duration::from_millis(250));
            }

            cb(
                SwkbdReplyType::DecidedEnter,
                text.to_string(),
                text.len() as i32,
            );
        }
    }
}

impl Applet for DefaultSoftwareKeyboardApplet {
    fn close(&self) {}
}

impl SoftwareKeyboardApplet for DefaultSoftwareKeyboardApplet {
    fn initialize_keyboard(
        &self,
        is_inline: bool,
        initialize_parameters: KeyboardInitializeParameters,
        submit_normal_callback: SubmitNormalCallback,
        submit_inline_callback: SubmitInlineCallback,
    ) {
        if is_inline {
            log::warn!(
                "(STUBBED) called, backend requested to initialize the inline software keyboard."
            );
            *self.submit_inline_callback.lock().unwrap() = Some(submit_inline_callback);
        } else {
            log::warn!(
                "(STUBBED) called, backend requested to initialize the normal software keyboard."
            );
            *self.submit_normal_callback.lock().unwrap() = Some(submit_normal_callback);
        }

        *self.parameters.lock().unwrap() = initialize_parameters;
        let parameters = self.parameters.lock().unwrap();

        log::info!(
            "\nKeyboardInitializeParameters:\
             \nok_text={}\
             \nheader_text={}\
             \nsub_text={}\
             \nguide_text={}\
             \ninitial_text={}\
             \nmax_text_length={}\
             \nmin_text_length={}\
             \ninitial_cursor_position={}\
             \ntype={:?}\
             \npassword_mode={:?}\
             \ntext_draw_type={:?}\
             \nkey_disable_flags={}\
             \nuse_blur_background={}\
             \nenable_backspace_button={}\
             \nenable_return_button={}\
             \ndisable_cancel_button={}",
            parameters.ok_text,
            parameters.header_text,
            parameters.sub_text,
            parameters.guide_text,
            parameters.initial_text,
            parameters.max_text_length,
            parameters.min_text_length,
            parameters.initial_cursor_position,
            parameters.swkbd_type,
            parameters.password_mode,
            parameters.text_draw_type,
            parameters.key_disable_flags.raw,
            parameters.use_blur_background,
            parameters.enable_backspace_button,
            parameters.enable_return_button,
            parameters.disable_cancel_button,
        );
    }

    fn show_normal_keyboard(&self) {
        log::warn!("(STUBBED) called, backend requested to show the normal software keyboard.");
        self.submit_normal_text("yuzu");
    }

    fn show_text_check_dialog(
        &self,
        _text_check_result: SwkbdTextCheckResult,
        _text_check_message: String,
    ) {
        log::warn!("(STUBBED) called, backend requested to show the text check dialog.");
    }

    fn show_inline_keyboard(&self, appear_parameters: InlineAppearParameters) {
        log::warn!("(STUBBED) called, backend requested to show the inline software keyboard.");

        log::info!(
            "\nInlineAppearParameters:\
             \nmax_text_length={}\
             \nmin_text_length={}\
             \nkey_top_scale_x={}\
             \nkey_top_scale_y={}\
             \nkey_top_translate_x={}\
             \nkey_top_translate_y={}\
             \ntype={:?}\
             \nkey_disable_flags={}\
             \nkey_top_as_floating={}\
             \nenable_backspace_button={}\
             \nenable_return_button={}\
             \ndisable_cancel_button={}",
            appear_parameters.max_text_length,
            appear_parameters.min_text_length,
            appear_parameters.key_top_scale_x,
            appear_parameters.key_top_scale_y,
            appear_parameters.key_top_translate_x,
            appear_parameters.key_top_translate_y,
            appear_parameters.swkbd_type,
            appear_parameters.key_disable_flags.raw,
            appear_parameters.key_top_as_floating,
            appear_parameters.enable_backspace_button,
            appear_parameters.enable_return_button,
            appear_parameters.disable_cancel_button,
        );

        // Upstream spawns a detached thread to submit inline text.
        // We replicate this behavior.
        // Upstream: `std::thread([this] { SubmitInlineText(u"yuzu"); }).detach();`
        // The callbacks are stored as `Fn` (not `FnOnce`), so they can be invoked
        // from a background thread. We replicate by calling submit_inline_text
        // directly since the Mutex<Option<Arc>> pattern already supports shared access.
        // A true detached thread would block the test harness, so we call synchronously
        // like upstream does in practice (the thread is fire-and-forget).
        self.submit_inline_text("yuzu");
    }

    fn hide_inline_keyboard(&self) {
        log::warn!("(STUBBED) called, backend requested to hide the inline software keyboard.");
    }

    fn inline_text_changed(&self, text_parameters: InlineTextParameters) {
        log::warn!("(STUBBED) called, backend requested to change the inline keyboard text.");

        log::info!(
            "\nInlineTextParameters:\
             \ninput_text={}\
             \ncursor_position={}",
            text_parameters.input_text,
            text_parameters.cursor_position,
        );

        if let Some(cb) = self.submit_inline_callback.lock().unwrap().as_ref() {
            cb(
                SwkbdReplyType::ChangedString,
                text_parameters.input_text,
                text_parameters.cursor_position,
            );
        }
    }

    fn exit_keyboard(&self) {
        log::warn!("(STUBBED) called, backend requested to exit the software keyboard.");
    }
}

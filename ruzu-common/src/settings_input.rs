//! Port of zuyu/src/common/settings_input.h and zuyu/src/common/settings_input.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

// ── NativeButton ────────────────────────────────────────────────────────────

pub mod native_button {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        A = 0,
        B,
        X,
        Y,
        LStick,
        RStick,
        L,
        R,
        ZL,
        ZR,
        Plus,
        Minus,
        DLeft,
        DUp,
        DRight,
        DDown,
        SLLeft,
        SRLeft,
        Home,
        Screenshot,
        SLRight,
        SRRight,
    }

    pub const NUM_BUTTONS: usize = 22;

    pub const BUTTON_HID_BEGIN: i32 = Values::A as i32;
    pub const BUTTON_NS_BEGIN: i32 = Values::Home as i32;
    pub const BUTTON_HID_END: i32 = BUTTON_NS_BEGIN;
    pub const BUTTON_NS_END: i32 = NUM_BUTTONS as i32;
    pub const NUM_BUTTONS_HID: i32 = BUTTON_HID_END - BUTTON_HID_BEGIN;
    pub const NUM_BUTTONS_NS: i32 = BUTTON_NS_END - BUTTON_NS_BEGIN;

    pub const MAPPING: [&str; NUM_BUTTONS] = [
        "button_a",
        "button_b",
        "button_x",
        "button_y",
        "button_lstick",
        "button_rstick",
        "button_l",
        "button_r",
        "button_zl",
        "button_zr",
        "button_plus",
        "button_minus",
        "button_dleft",
        "button_dup",
        "button_dright",
        "button_ddown",
        "button_slleft",
        "button_srleft",
        "button_home",
        "button_screenshot",
        "button_slright",
        "button_srright",
    ];
}

// ── NativeAnalog ────────────────────────────────────────────────────────────

pub mod native_analog {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        LStick = 0,
        RStick,
    }

    pub const NUM_ANALOGS: usize = 2;

    pub const STICK_HID_BEGIN: i32 = Values::LStick as i32;
    pub const STICK_HID_END: i32 = NUM_ANALOGS as i32;

    pub const MAPPING: [&str; NUM_ANALOGS] = ["lstick", "rstick"];
}

// ── NativeTrigger ───────────────────────────────────────────────────────────

pub mod native_trigger {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        LTrigger = 0,
        RTrigger,
    }

    pub const NUM_TRIGGERS: usize = 2;

    pub const TRIGGER_HID_BEGIN: i32 = Values::LTrigger as i32;
    pub const TRIGGER_HID_END: i32 = NUM_TRIGGERS as i32;
}

// ── NativeVibration ─────────────────────────────────────────────────────────

pub mod native_vibration {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        LeftVibrationDevice = 0,
        RightVibrationDevice,
    }

    pub const NUM_VIBRATIONS: usize = 2;

    pub const VIBRATION_HID_BEGIN: i32 = Values::LeftVibrationDevice as i32;
    pub const VIBRATION_HID_END: i32 = NUM_VIBRATIONS as i32;
    pub const NUM_VIBRATIONS_HID: usize = NUM_VIBRATIONS;

    pub const MAPPING: [&str; NUM_VIBRATIONS] = [
        "left_vibration_device",
        "right_vibration_device",
    ];
}

// ── NativeMotion ────────────────────────────────────────────────────────────

pub mod native_motion {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        MotionLeft = 0,
        MotionRight,
    }

    pub const NUM_MOTIONS: usize = 2;

    pub const MOTION_HID_BEGIN: i32 = Values::MotionLeft as i32;
    pub const MOTION_HID_END: i32 = NUM_MOTIONS as i32;
    pub const NUM_MOTIONS_HID: usize = NUM_MOTIONS;

    pub const MAPPING: [&str; NUM_MOTIONS] = ["motionleft", "motionright"];
}

// ── NativeMouseButton ───────────────────────────────────────────────────────

pub mod native_mouse_button {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        Left = 0,
        Right,
        Middle,
        Forward,
        Back,
    }

    pub const NUM_MOUSE_BUTTONS: usize = 5;

    pub const MOUSE_HID_BEGIN: i32 = Values::Left as i32;
    pub const MOUSE_HID_END: i32 = NUM_MOUSE_BUTTONS as i32;
    pub const NUM_MOUSE_HID: usize = NUM_MOUSE_BUTTONS;

    pub const MAPPING: [&str; NUM_MOUSE_BUTTONS] = [
        "left", "right", "middle", "forward", "back",
    ];
}

// ── NativeMouseWheel ────────────────────────────────────────────────────────

pub mod native_mouse_wheel {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Values {
        X = 0,
        Y,
    }

    pub const NUM_MOUSE_WHEELS: usize = 2;

    pub const MAPPING: [&str; NUM_MOUSE_WHEELS] = ["x", "y"];
}

// ── NativeKeyboard ──────────────────────────────────────────────────────────

pub mod native_keyboard {
    /// Keyboard key codes matching the HID spec used by the Switch.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    #[allow(non_camel_case_types)]
    pub enum Keys {
        None = 0,

        A = 4,
        B = 5,
        C = 6,
        D = 7,
        E = 8,
        F = 9,
        G = 10,
        H = 11,
        I = 12,
        J = 13,
        K = 14,
        L = 15,
        M = 16,
        N = 17,
        O = 18,
        P = 19,
        Q = 20,
        R = 21,
        S = 22,
        T = 23,
        U = 24,
        V = 25,
        W = 26,
        X = 27,
        Y = 28,
        Z = 29,
        N1 = 30,
        N2 = 31,
        N3 = 32,
        N4 = 33,
        N5 = 34,
        N6 = 35,
        N7 = 36,
        N8 = 37,
        N9 = 38,
        N0 = 39,
        Return = 40,
        Escape = 41,
        Backspace = 42,
        Tab = 43,
        Space = 44,
        Minus = 45,
        Plus = 46,
        OpenBracket = 47,
        CloseBracket = 48,
        Pipe = 49,
        Tilde = 50,
        Semicolon = 51,
        Quote = 52,
        Backquote = 53,
        Comma = 54,
        Period = 55,
        Slash = 56,
        CapsLockKey = 57,

        F1 = 58,
        F2 = 59,
        F3 = 60,
        F4 = 61,
        F5 = 62,
        F6 = 63,
        F7 = 64,
        F8 = 65,
        F9 = 66,
        F10 = 67,
        F11 = 68,
        F12 = 69,

        PrintScreen = 70,
        ScrollLockKey = 71,
        Pause = 72,
        Insert = 73,
        Home = 74,
        PageUp = 75,
        Delete = 76,
        End = 77,
        PageDown = 78,
        Right = 79,
        Left = 80,
        Down = 81,
        Up = 82,

        NumLockKey = 83,
        KPSlash = 84,
        KPAsterisk = 85,
        KPMinus = 86,
        KPPlus = 87,
        KPEnter = 88,
        KP1 = 89,
        KP2 = 90,
        KP3 = 91,
        KP4 = 92,
        KP5 = 93,
        KP6 = 94,
        KP7 = 95,
        KP8 = 96,
        KP9 = 97,
        KP0 = 98,
        KPDot = 99,

        Key102 = 100,
        Compose = 101,
        Power = 102,
        KPEqual = 103,

        F13 = 104,
        F14 = 105,
        F15 = 106,
        F16 = 107,
        F17 = 108,
        F18 = 109,
        F19 = 110,
        F20 = 111,
        F21 = 112,
        F22 = 113,
        F23 = 114,
        F24 = 115,

        Open = 116,
        Help = 117,
        Properties = 118,
        Front = 119,
        Stop = 120,
        Repeat = 121,
        Undo = 122,
        Cut = 123,
        Copy = 124,
        Paste = 125,
        Find = 126,
        Mute = 127,
        VolumeUp = 128,
        VolumeDown = 129,
        CapsLockActive = 130,
        NumLockActive = 131,
        ScrollLockActive = 132,
        KPComma = 133,

        Ro = 0x87,
        KatakanaHiragana = 0x88,
        Yen = 0x89,
        Henkan = 0x8a,
        Muhenkan = 0x8b,
        NumPadCommaPc98 = 0x8c,

        HangulEnglish = 0x90,
        Hanja = 0x91,
        KatakanaKey = 0x92,
        HiraganaKey = 0x93,
        ZenkakuHankaku = 0x94,

        LeftControlKey = 0xE0,
        LeftShiftKey = 0xE1,
        LeftAltKey = 0xE2,
        LeftMetaKey = 0xE3,
        RightControlKey = 0xE4,
        RightShiftKey = 0xE5,
        RightAltKey = 0xE6,
        RightMetaKey = 0xE7,

        MediaPlayPause = 0xE8,
        MediaStopCD = 0xE9,
        MediaPrevious = 0xEA,
        MediaNext = 0xEB,
        MediaEject = 0xEC,
        MediaVolumeUp = 0xED,
        MediaVolumeDown = 0xEE,
        MediaMute = 0xEF,
        MediaWebsite = 0xF0,
        MediaBack = 0xF1,
        MediaForward = 0xF2,
        MediaStop = 0xF3,
        MediaFind = 0xF4,
        MediaScrollUp = 0xF5,
        MediaScrollDown = 0xF6,
        MediaEdit = 0xF7,
        MediaSleep = 0xF8,
        MediaCoffee = 0xF9,
        MediaRefresh = 0xFA,
        MediaCalculator = 0xFB,
    }

    pub const NUM_KEYBOARD_KEYS: usize = 0xFC;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(i32)]
    pub enum Modifiers {
        LeftControl = 0,
        LeftShift,
        LeftAlt,
        LeftMeta,
        RightControl,
        RightShift,
        RightAlt,
        RightMeta,
        CapsLock,
        ScrollLock,
        NumLock,
        Katakana,
        Hiragana,
    }

    pub const NUM_KEYBOARD_MODS: usize = 13;

    pub const KEYBOARD_KEYS_HID_BEGIN: i32 = Keys::None as i32;
    pub const KEYBOARD_KEYS_HID_END: usize = NUM_KEYBOARD_KEYS;
    pub const NUM_KEYBOARD_KEYS_HID: usize = NUM_KEYBOARD_KEYS;

    pub const KEYBOARD_MODS_HID_BEGIN: i32 = Modifiers::LeftControl as i32;
    pub const KEYBOARD_MODS_HID_END: usize = NUM_KEYBOARD_MODS;
    pub const NUM_KEYBOARD_MODS_HID: usize = NUM_KEYBOARD_MODS;
}

// ── Type aliases ────────────────────────────────────────────────────────────

/// Raw analog stick configuration strings.
pub type AnalogsRaw = [String; native_analog::NUM_ANALOGS];

/// Raw button configuration strings.
pub type ButtonsRaw = [String; native_button::NUM_BUTTONS];

/// Raw motion configuration strings.
pub type MotionsRaw = [String; native_motion::NUM_MOTIONS];

/// Raw ringcon configuration string.
pub type RingconRaw = String;

// ── Color constants ─────────────────────────────────────────────────────────

pub const JOYCON_BODY_NEON_RED: u32 = 0xFF3C28;
pub const JOYCON_BUTTONS_NEON_RED: u32 = 0x1E0A0A;
pub const JOYCON_BODY_NEON_BLUE: u32 = 0x0AB9E6;
pub const JOYCON_BUTTONS_NEON_BLUE: u32 = 0x001E1E;

// ── Controller type ─────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerType {
    ProController,
    DualJoyconDetached,
    LeftJoycon,
    RightJoycon,
    Handheld,
    GameCube,
    Pokeball,
    NES,
    SNES,
    N64,
    SegaGenesis,
}

impl Default for ControllerType {
    fn default() -> Self {
        Self::ProController
    }
}

// ── PlayerInput ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct PlayerInput {
    pub connected: bool,
    pub controller_type: ControllerType,
    pub buttons: ButtonsRaw,
    pub analogs: AnalogsRaw,
    pub motions: MotionsRaw,

    pub vibration_enabled: bool,
    pub vibration_strength: i32,

    pub body_color_left: u32,
    pub body_color_right: u32,
    pub button_color_left: u32,
    pub button_color_right: u32,
    pub profile_name: String,

    pub use_system_vibrator: bool,
}

impl Default for PlayerInput {
    fn default() -> Self {
        Self {
            connected: false,
            controller_type: ControllerType::ProController,
            buttons: Default::default(),
            analogs: Default::default(),
            motions: Default::default(),
            vibration_enabled: true,
            vibration_strength: 100,
            body_color_left: JOYCON_BODY_NEON_BLUE,
            body_color_right: JOYCON_BODY_NEON_RED,
            button_color_left: JOYCON_BUTTONS_NEON_BLUE,
            button_color_right: JOYCON_BUTTONS_NEON_RED,
            profile_name: String::new(),
            use_system_vibrator: false,
        }
    }
}

// ── TouchscreenInput ────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct TouchscreenInput {
    pub enabled: bool,
    pub device: String,
    pub finger: u32,
    pub diameter_x: u32,
    pub diameter_y: u32,
    pub rotation_angle: u32,
}

impl Default for TouchscreenInput {
    fn default() -> Self {
        Self {
            enabled: false,
            device: String::new(),
            finger: 0,
            diameter_x: 0,
            diameter_y: 0,
            rotation_angle: 0,
        }
    }
}

// ── TouchFromButtonMap ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct TouchFromButtonMap {
    pub name: String,
    pub buttons: Vec<String>,
}

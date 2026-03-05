//! Port of zuyu/src/common/settings_enums.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

/// Macro to generate enum types with string canonicalization support.
/// Each enum is `#[repr(u32)]` to match the C++ `enum class : u32`.
macro_rules! settings_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $($variant:ident),+ $(,)?
        }
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u32)]
        $vis enum $name {
            $($variant),+
        }

        impl $name {
            /// Returns the string name of this enum variant.
            pub fn canonicalize(self) -> &'static str {
                match self {
                    $(Self::$variant => stringify!($variant)),+
                }
            }

            /// Parse a variant from its string name (case-sensitive).
            pub fn from_string(s: &str) -> Option<Self> {
                match s {
                    $(stringify!($variant) => Some(Self::$variant),)+
                    _ => None,
                }
            }

            /// Parse a variant from its numeric value.
            pub fn from_u32(val: u32) -> Option<Self> {
                let mut _idx = 0u32;
                $(
                    if val == _idx {
                        return Some(Self::$variant);
                    }
                    _idx += 1;
                )+
                None
            }

            /// Returns all variants as a slice of (name, value) pairs.
            pub fn canonicalizations() -> &'static [(&'static str, Self)] {
                &[
                    $((stringify!($variant), Self::$variant)),+
                ]
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.canonicalize())
            }
        }

        impl Default for $name {
            fn default() -> Self {
                // First variant is the default
                settings_enum!(@first $($variant),+)
            }
        }
    };
    (@first $first:ident $(, $rest:ident)*) => {
        Self::$first
    };
}

// AudioEngine has special canonicalizations (lowercase), defined separately
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u32)]
pub enum AudioEngine {
    Auto = 0,
    Cubeb = 1,
    Sdl2 = 2,
    Null = 3,
    Oboe = 4,
}

impl AudioEngine {
    pub fn canonicalize(self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::Cubeb => "cubeb",
            Self::Sdl2 => "sdl2",
            Self::Null => "null",
            Self::Oboe => "oboe",
        }
    }

    pub fn from_string(s: &str) -> Option<Self> {
        match s {
            "auto" => Some(Self::Auto),
            "cubeb" => Some(Self::Cubeb),
            "sdl2" => Some(Self::Sdl2),
            "null" => Some(Self::Null),
            "oboe" => Some(Self::Oboe),
            _ => None,
        }
    }

    pub fn from_u32(val: u32) -> Option<Self> {
        match val {
            0 => Some(Self::Auto),
            1 => Some(Self::Cubeb),
            2 => Some(Self::Sdl2),
            3 => Some(Self::Null),
            4 => Some(Self::Oboe),
            _ => None,
        }
    }
}

impl Default for AudioEngine {
    fn default() -> Self {
        Self::Auto
    }
}

impl std::fmt::Display for AudioEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.canonicalize())
    }
}

settings_enum! {
    pub enum AudioMode {
        Mono,
        Stereo,
        Surround,
    }
}

settings_enum! {
    pub enum Language {
        Japanese,
        EnglishAmerican,
        French,
        German,
        Italian,
        Spanish,
        Chinese,
        Korean,
        Dutch,
        Portuguese,
        Russian,
        Taiwanese,
        EnglishBritish,
        FrenchCanadian,
        SpanishLatin,
        ChineseSimplified,
        ChineseTraditional,
        PortugueseBrazilian,
    }
}

settings_enum! {
    pub enum Region {
        Japan,
        Usa,
        Europe,
        Australia,
        China,
        Korea,
        Taiwan,
    }
}

settings_enum! {
    pub enum TimeZone {
        Auto, Default, Cet, Cst6Cdt, Cuba, Eet, Egypt, Eire, Est, Est5Edt, Gb, GbEire, Gmt,
        GmtPlusZero, GmtMinusZero, GmtZero, Greenwich, Hongkong, Hst, Iceland, Iran, Israel,
        Jamaica, Japan, Kwajalein, Libya, Met, Mst, Mst7Mdt, Navajo, Nz, NzChat, Poland,
        Portugal, Prc, Pst8Pdt, Roc, Rok, Singapore, Turkey, Uct, Universal, Utc, WSu, Wet, Zulu,
    }
}

settings_enum! {
    pub enum AnisotropyMode {
        Automatic,
        Default,
        X2,
        X4,
        X8,
        X16,
    }
}

settings_enum! {
    pub enum AstcDecodeMode {
        Cpu,
        Gpu,
        CpuAsynchronous,
    }
}

settings_enum! {
    pub enum AstcRecompression {
        Uncompressed,
        Bc1,
        Bc3,
    }
}

settings_enum! {
    pub enum VSyncMode {
        Immediate,
        Mailbox,
        Fifo,
        FifoRelaxed,
    }
}

settings_enum! {
    pub enum VramUsageMode {
        Conservative,
        Aggressive,
    }
}

settings_enum! {
    pub enum RendererBackend {
        OpenGL,
        Vulkan,
        Null,
    }
}

impl RendererBackend {
    /// Compatibility helper: parse from string or numeric value used in config files.
    pub fn from_str_or_default(s: &str) -> Self {
        match s.trim().to_lowercase().as_str() {
            "0" | "opengl" => Self::OpenGL,
            "1" | "vulkan" => Self::Vulkan,
            "2" | "null" => Self::Null,
            _ => Self::Vulkan,
        }
    }
}

settings_enum! {
    pub enum ShaderBackend {
        Glsl,
        Glasm,
        SpirV,
    }
}

settings_enum! {
    pub enum GpuAccuracy {
        Normal,
        High,
        Extreme,
    }
}

settings_enum! {
    pub enum CpuBackend {
        Dynarmic,
        Nce,
    }
}

settings_enum! {
    pub enum CpuAccuracy {
        Auto,
        Accurate,
        Unsafe,
        Paranoid,
    }
}

settings_enum! {
    pub enum MemoryLayout {
        Memory4Gb,
        Memory6Gb,
        Memory8Gb,
    }
}

settings_enum! {
    pub enum ConfirmStop {
        AskAlways,
        AskBasedOnGame,
        AskNever,
    }
}

settings_enum! {
    pub enum FullscreenMode {
        Borderless,
        Exclusive,
    }
}

settings_enum! {
    pub enum NvdecEmulation {
        Off,
        Cpu,
        Gpu,
    }
}

settings_enum! {
    pub enum ResolutionSetup {
        Res1_2X,
        Res3_4X,
        Res1X,
        Res3_2X,
        Res2X,
        Res3X,
        Res4X,
        Res5X,
        Res6X,
        Res7X,
        Res8X,
    }
}

settings_enum! {
    pub enum ScalingFilter {
        NearestNeighbor,
        Bilinear,
        Bicubic,
        Gaussian,
        ScaleForce,
        Fsr,
        MaxEnum,
    }
}

settings_enum! {
    pub enum AntiAliasing {
        None,
        Fxaa,
        Smaa,
        MaxEnum,
    }
}

settings_enum! {
    pub enum AspectRatio {
        R16_9,
        R4_3,
        R21_9,
        R16_10,
        Stretch,
    }
}

settings_enum! {
    pub enum ConsoleMode {
        Handheld,
        Docked,
    }
}

settings_enum! {
    pub enum AppletMode {
        HLE,
        LLE,
    }
}

/// Category for settings, matching the C++ `enum class Category : u32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u32)]
pub enum Category {
    Android = 0,
    Audio,
    Core,
    Cpu,
    CpuDebug,
    CpuUnsafe,
    Overlay,
    Renderer,
    RendererAdvanced,
    RendererDebug,
    System,
    SystemAudio,
    DataStorage,
    Debugging,
    DebuggingGraphics,
    GpuDriver,
    Miscellaneous,
    Network,
    WebService,
    AddOns,
    Controls,
    Ui,
    UiAudio,
    UiGeneral,
    UiLayout,
    UiGameList,
    Screenshots,
    Shortcuts,
    Multiplayer,
    Services,
    Paths,
    Linux,
    LibraryApplet,
    MaxEnum,
}

impl Category {
    /// Translate a category into its INI section name, matching the C++ `TranslateCategory`.
    pub fn translate(&self) -> &'static str {
        match self {
            Category::Android => "Android",
            Category::Audio => "Audio",
            Category::Core => "Core",
            Category::Cpu | Category::CpuDebug | Category::CpuUnsafe => "Cpu",
            Category::Overlay => "Overlay",
            Category::Renderer | Category::RendererAdvanced | Category::RendererDebug => {
                "Renderer"
            }
            Category::System | Category::SystemAudio => "System",
            Category::DataStorage => "Data Storage",
            Category::Debugging | Category::DebuggingGraphics => "Debugging",
            Category::GpuDriver => "GpuDriver",
            Category::LibraryApplet => "LibraryApplet",
            Category::Miscellaneous => "Miscellaneous",
            Category::Network => "Network",
            Category::WebService => "WebService",
            Category::AddOns => "DisabledAddOns",
            Category::Controls => "Controls",
            Category::Ui | Category::UiGeneral => "UI",
            Category::UiAudio => "UiAudio",
            Category::UiLayout => "UILayout",
            Category::UiGameList => "UIGameList",
            Category::Screenshots => "Screenshots",
            Category::Shortcuts => "Shortcuts",
            Category::Multiplayer => "Multiplayer",
            Category::Services => "Services",
            Category::Paths => "Paths",
            Category::Linux => "Linux",
            Category::MaxEnum => "Miscellaneous",
        }
    }
}

//! Port of zuyu/src/common/settings.h and zuyu/src/common/settings.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Contains the global `Values` struct with ALL emulator settings, plus
//! helper functions matching the C++ free functions in `Settings` namespace.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};

use log::{info, warn};

use crate::settings_common::{
    InputSetting, Setting, Specialization, SwitchableSetting,
};

// Re-export the types that consumers most commonly need.
pub use crate::settings_enums::{
    AnisotropyMode, AntiAliasing, AppletMode, AspectRatio, AstcDecodeMode, AstcRecompression,
    AudioEngine, AudioMode, Category, ConfirmStop, ConsoleMode, CpuAccuracy, CpuBackend,
    FullscreenMode, GpuAccuracy, Language, MemoryLayout, NvdecEmulation, Region, RendererBackend,
    ResolutionSetup, ScalingFilter, ShaderBackend, TimeZone, VramUsageMode, VSyncMode,
};
pub use crate::settings_input::{
    AnalogsRaw, ButtonsRaw, PlayerInput, RingconRaw, TouchFromButtonMap, TouchscreenInput,
};

// Keep backward-compat aliases that the old code used.
pub type SystemLanguage = Language;
pub type SystemRegion = Region;

impl Language {
    /// Compatibility: parse from numeric index (used by config loader).
    pub fn from_index(index: u32) -> Self {
        Self::from_u32(index).unwrap_or(Self::EnglishAmerican)
    }
}

impl Region {
    /// Compatibility: parse from numeric index (used by config loader).
    pub fn from_index(index: u32) -> Self {
        Self::from_u32(index).unwrap_or(Self::Usa)
    }
}

// ── ResolutionScalingInfo ───────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct ResolutionScalingInfo {
    pub up_scale: u32,
    pub down_shift: u32,
    pub up_factor: f32,
    pub down_factor: f32,
    pub active: bool,
    pub downscale: bool,
}

impl Default for ResolutionScalingInfo {
    fn default() -> Self {
        Self {
            up_scale: 1,
            down_shift: 0,
            up_factor: 1.0,
            down_factor: 1.0,
            active: false,
            downscale: false,
        }
    }
}

impl ResolutionScalingInfo {
    pub fn scale_up_i32(&self, value: i32) -> i32 {
        if value == 0 {
            return 0;
        }
        ((value * self.up_scale as i32) >> self.down_shift as i32).max(1)
    }

    pub fn scale_up_u32(&self, value: u32) -> u32 {
        if value == 0 {
            return 0;
        }
        ((value * self.up_scale) >> self.down_shift).max(1)
    }
}

// ── Values ──────────────────────────────────────────────────────────────────

/// The main settings container matching C++ `Settings::Values`.
/// All emulator settings live here.
pub struct Values {
    // ── Applet ──────────────────────────────────────────────────────────
    pub cabinet_applet_mode: Setting<AppletMode>,
    pub controller_applet_mode: Setting<AppletMode>,
    pub data_erase_applet_mode: Setting<AppletMode>,
    pub error_applet_mode: Setting<AppletMode>,
    pub net_connect_applet_mode: Setting<AppletMode>,
    pub player_select_applet_mode: Setting<AppletMode>,
    pub swkbd_applet_mode: Setting<AppletMode>,
    pub mii_edit_applet_mode: Setting<AppletMode>,
    pub web_applet_mode: Setting<AppletMode>,
    pub shop_applet_mode: Setting<AppletMode>,
    pub photo_viewer_applet_mode: Setting<AppletMode>,
    pub offline_web_applet_mode: Setting<AppletMode>,
    pub login_share_applet_mode: Setting<AppletMode>,
    pub wifi_web_auth_applet_mode: Setting<AppletMode>,
    pub my_page_applet_mode: Setting<AppletMode>,

    // ── Audio ───────────────────────────────────────────────────────────
    pub sink_id: SwitchableSetting<AudioEngine>,
    pub audio_output_device_id: SwitchableSetting<String>,
    pub audio_input_device_id: SwitchableSetting<String>,
    pub sound_index: SwitchableSetting<AudioMode>,
    pub volume: SwitchableSetting<u8>,
    pub audio_muted: Setting<bool>,
    pub dump_audio_commands: Setting<bool>,

    // ── Core ────────────────────────────────────────────────────────────
    pub use_multi_core: SwitchableSetting<bool>,
    pub memory_layout_mode: SwitchableSetting<MemoryLayout>,
    pub use_speed_limit: SwitchableSetting<bool>,
    pub speed_limit: SwitchableSetting<u16>,

    // ── CPU ─────────────────────────────────────────────────────────────
    pub cpu_backend: SwitchableSetting<CpuBackend>,
    pub cpu_accuracy: SwitchableSetting<CpuAccuracy>,
    pub cpu_debug_mode: SwitchableSetting<bool>,

    pub cpuopt_page_tables: Setting<bool>,
    pub cpuopt_block_linking: Setting<bool>,
    pub cpuopt_return_stack_buffer: Setting<bool>,
    pub cpuopt_fast_dispatcher: Setting<bool>,
    pub cpuopt_context_elimination: Setting<bool>,
    pub cpuopt_const_prop: Setting<bool>,
    pub cpuopt_misc_ir: Setting<bool>,
    pub cpuopt_reduce_misalign_checks: Setting<bool>,
    pub cpuopt_fastmem: SwitchableSetting<bool>,
    pub cpuopt_fastmem_exclusives: SwitchableSetting<bool>,
    pub cpuopt_recompile_exclusives: Setting<bool>,
    pub cpuopt_ignore_memory_aborts: Setting<bool>,

    pub cpuopt_unsafe_unfuse_fma: SwitchableSetting<bool>,
    pub cpuopt_unsafe_reduce_fp_error: SwitchableSetting<bool>,
    pub cpuopt_unsafe_ignore_standard_fpcr: SwitchableSetting<bool>,
    pub cpuopt_unsafe_inaccurate_nan: SwitchableSetting<bool>,
    pub cpuopt_unsafe_fastmem_check: SwitchableSetting<bool>,
    pub cpuopt_unsafe_ignore_global_monitor: SwitchableSetting<bool>,

    // ── Renderer ────────────────────────────────────────────────────────
    pub renderer_backend: SwitchableSetting<RendererBackend>,
    pub shader_backend: SwitchableSetting<ShaderBackend>,
    pub vulkan_device: SwitchableSetting<i32>,

    pub use_disk_shader_cache: SwitchableSetting<bool>,
    pub use_asynchronous_gpu_emulation: SwitchableSetting<bool>,
    pub accelerate_astc: SwitchableSetting<AstcDecodeMode>,
    pub vsync_mode: SwitchableSetting<VSyncMode>,
    pub nvdec_emulation: SwitchableSetting<NvdecEmulation>,
    pub fullscreen_mode: SwitchableSetting<FullscreenMode>,
    pub aspect_ratio: SwitchableSetting<AspectRatio>,

    pub resolution_info: ResolutionScalingInfo,
    pub resolution_setup: SwitchableSetting<ResolutionSetup>,
    pub scaling_filter: SwitchableSetting<ScalingFilter>,
    pub anti_aliasing: SwitchableSetting<AntiAliasing>,
    pub fsr_sharpening_slider: SwitchableSetting<i32>,

    pub bg_red: SwitchableSetting<u8>,
    pub bg_green: SwitchableSetting<u8>,
    pub bg_blue: SwitchableSetting<u8>,

    // ── Renderer Advanced ───────────────────────────────────────────────
    pub gpu_accuracy: SwitchableSetting<GpuAccuracy>,
    pub current_gpu_accuracy: GpuAccuracy,
    pub max_anisotropy: SwitchableSetting<AnisotropyMode>,
    pub astc_recompression: SwitchableSetting<AstcRecompression>,
    pub vram_usage_mode: SwitchableSetting<VramUsageMode>,
    pub async_presentation: SwitchableSetting<bool>,
    pub renderer_force_max_clock: SwitchableSetting<bool>,
    pub use_reactive_flushing: SwitchableSetting<bool>,
    pub use_asynchronous_shaders: SwitchableSetting<bool>,
    pub use_fast_gpu_time: SwitchableSetting<bool>,
    pub use_vulkan_driver_pipeline_cache: SwitchableSetting<bool>,
    pub enable_compute_pipelines: SwitchableSetting<bool>,
    pub use_video_framerate: SwitchableSetting<bool>,
    pub barrier_feedback_loops: SwitchableSetting<bool>,

    // ── Renderer Debug ──────────────────────────────────────────────────
    pub renderer_debug: Setting<bool>,
    pub renderer_shader_feedback: Setting<bool>,
    pub enable_nsight_aftermath: Setting<bool>,
    pub disable_shader_loop_safety_checks: Setting<bool>,
    pub enable_renderdoc_hotkey: Setting<bool>,
    pub disable_buffer_reorder: Setting<bool>,

    // ── System ──────────────────────────────────────────────────────────
    pub language_index: SwitchableSetting<Language>,
    pub region_index: SwitchableSetting<Region>,
    pub time_zone_index: SwitchableSetting<TimeZone>,
    pub custom_rtc_enabled: SwitchableSetting<bool>,
    pub custom_rtc: SwitchableSetting<i64>,
    pub custom_rtc_offset: SwitchableSetting<i64>,
    pub rng_seed_enabled: SwitchableSetting<bool>,
    pub rng_seed: SwitchableSetting<u32>,
    pub device_name: Setting<String>,
    pub current_user: Setting<i32>,
    pub use_docked_mode: SwitchableSetting<ConsoleMode>,

    // ── Linux ───────────────────────────────────────────────────────────
    pub enable_gamemode: SwitchableSetting<bool>,

    // ── Controls ────────────────────────────────────────────────────────
    pub players: InputSetting<[PlayerInput; 10]>,

    pub enable_raw_input: Setting<bool>,
    pub controller_navigation: Setting<bool>,
    pub enable_joycon_driver: Setting<bool>,
    pub enable_procon_driver: Setting<bool>,

    pub vibration_enabled: SwitchableSetting<bool>,
    pub enable_accurate_vibrations: SwitchableSetting<bool>,
    pub motion_enabled: SwitchableSetting<bool>,

    pub udp_input_servers: Setting<String>,
    pub enable_udp_controller: Setting<bool>,

    pub pause_tas_on_load: Setting<bool>,
    pub tas_enable: Setting<bool>,
    pub tas_loop: Setting<bool>,

    pub mouse_panning: Setting<bool>,
    pub mouse_panning_sensitivity: Setting<u8>,
    pub mouse_enabled: Setting<bool>,
    pub mouse_panning_x_sensitivity: Setting<u8>,
    pub mouse_panning_y_sensitivity: Setting<u8>,
    pub mouse_panning_deadzone_counterweight: Setting<u8>,
    pub mouse_panning_decay_strength: Setting<u8>,
    pub mouse_panning_min_decay: Setting<u8>,

    pub emulate_analog_keyboard: Setting<bool>,
    pub keyboard_enabled: Setting<bool>,

    pub debug_pad_enabled: Setting<bool>,
    pub debug_pad_buttons: ButtonsRaw,
    pub debug_pad_analogs: AnalogsRaw,

    pub touchscreen: TouchscreenInput,
    pub touch_device: Setting<String>,
    pub touch_from_button_map_index: Setting<i32>,
    pub touch_from_button_maps: Vec<TouchFromButtonMap>,

    pub enable_ring_controller: Setting<bool>,
    pub ringcon_analogs: RingconRaw,

    pub enable_ir_sensor: Setting<bool>,
    pub ir_sensor_device: Setting<String>,

    pub random_amiibo_id: Setting<bool>,

    // ── Data Storage ────────────────────────────────────────────────────
    pub use_virtual_sd: Setting<bool>,
    pub gamecard_inserted: Setting<bool>,
    pub gamecard_current_game: Setting<bool>,
    pub gamecard_path: Setting<String>,

    // ── Debugging ───────────────────────────────────────────────────────
    pub record_frame_times: bool,
    pub use_gdbstub: Setting<bool>,
    pub gdbstub_port: Setting<u16>,
    pub program_args: Setting<String>,
    pub dump_exefs: Setting<bool>,
    pub dump_nso: Setting<bool>,
    pub dump_shaders: Setting<bool>,
    pub dump_macros: Setting<bool>,
    pub enable_fs_access_log: Setting<bool>,
    pub reporting_services: Setting<bool>,
    pub quest_flag: Setting<bool>,
    pub disable_macro_jit: Setting<bool>,
    pub disable_macro_hle: Setting<bool>,
    pub extended_logging: Setting<bool>,
    pub use_debug_asserts: Setting<bool>,
    pub use_auto_stub: Setting<bool>,
    pub enable_all_controllers: Setting<bool>,
    pub perform_vulkan_check: Setting<bool>,

    // ── Miscellaneous ───────────────────────────────────────────────────
    pub log_filter: Setting<String>,
    pub use_dev_keys: Setting<bool>,

    // ── Network ─────────────────────────────────────────────────────────
    pub network_interface: Setting<String>,

    // ── WebService ──────────────────────────────────────────────────────
    pub enable_telemetry: Setting<bool>,
    pub web_api_url: Setting<String>,
    pub yuzu_username: Setting<String>,
    pub yuzu_token: Setting<String>,

    // ── Add-Ons ─────────────────────────────────────────────────────────
    pub disabled_addons: HashMap<u64, Vec<String>>,

    // ── Extra fields (not in C++ Values but kept for backward compat) ──
    pub title_id: u64,
    pub keys_dir: Option<std::path::PathBuf>,
    pub games_dir: Option<std::path::PathBuf>,
}

impl Default for Values {
    fn default() -> Self {
        use Category::*;

        Self {
            // Applet
            cabinet_applet_mode: Setting::new(AppletMode::LLE, "cabinet_applet_mode", LibraryApplet),
            controller_applet_mode: Setting::new(AppletMode::HLE, "controller_applet_mode", LibraryApplet),
            data_erase_applet_mode: Setting::new(AppletMode::HLE, "data_erase_applet_mode", LibraryApplet),
            error_applet_mode: Setting::new(AppletMode::LLE, "error_applet_mode", LibraryApplet),
            net_connect_applet_mode: Setting::new(AppletMode::HLE, "net_connect_applet_mode", LibraryApplet),
            player_select_applet_mode: Setting::new(AppletMode::HLE, "player_select_applet_mode", LibraryApplet),
            swkbd_applet_mode: Setting::new(AppletMode::LLE, "swkbd_applet_mode", LibraryApplet),
            mii_edit_applet_mode: Setting::new(AppletMode::LLE, "mii_edit_applet_mode", LibraryApplet),
            web_applet_mode: Setting::new(AppletMode::HLE, "web_applet_mode", LibraryApplet),
            shop_applet_mode: Setting::new(AppletMode::HLE, "shop_applet_mode", LibraryApplet),
            photo_viewer_applet_mode: Setting::new(AppletMode::LLE, "photo_viewer_applet_mode", LibraryApplet),
            offline_web_applet_mode: Setting::new(AppletMode::LLE, "offline_web_applet_mode", LibraryApplet),
            login_share_applet_mode: Setting::new(AppletMode::HLE, "login_share_applet_mode", LibraryApplet),
            wifi_web_auth_applet_mode: Setting::new(AppletMode::HLE, "wifi_web_auth_applet_mode", LibraryApplet),
            my_page_applet_mode: Setting::new(AppletMode::LLE, "my_page_applet_mode", LibraryApplet),

            // Audio
            sink_id: SwitchableSetting::with_options(AudioEngine::Auto, "output_engine", Audio, Specialization::RUNTIME_LIST, true, false),
            audio_output_device_id: SwitchableSetting::with_options("auto".to_string(), "output_device", Audio, Specialization::RUNTIME_LIST, true, false),
            audio_input_device_id: SwitchableSetting::with_options("auto".to_string(), "input_device", Audio, Specialization::RUNTIME_LIST, true, false),
            sound_index: SwitchableSetting::ranged(AudioMode::Stereo, AudioMode::Mono, AudioMode::Surround, "sound_index", SystemAudio),
            volume: SwitchableSetting::ranged_with_options(100, 0, 200, "volume", Audio, Specialization::SCALAR | Specialization::PERCENTAGE, true, true),
            audio_muted: Setting::with_options(false, "audio_muted", Audio, Specialization::DEFAULT, true, true),
            dump_audio_commands: Setting::new(false, "dump_audio_commands", Audio),

            // Core
            use_multi_core: SwitchableSetting::new(true, "use_multi_core", Core),
            memory_layout_mode: SwitchableSetting::ranged(MemoryLayout::Memory4Gb, MemoryLayout::Memory4Gb, MemoryLayout::Memory8Gb, "memory_layout_mode", Core),
            use_speed_limit: SwitchableSetting::with_options(true, "use_speed_limit", Core, Specialization::PAIRED, false, true),
            speed_limit: SwitchableSetting::ranged_with_options(100, 0, 9999, "speed_limit", Core, Specialization::COUNTABLE | Specialization::PERCENTAGE, true, true),

            // CPU
            cpu_backend: SwitchableSetting::ranged(CpuBackend::Dynarmic, CpuBackend::Dynarmic, CpuBackend::Dynarmic, "cpu_backend", Cpu),
            cpu_accuracy: SwitchableSetting::ranged(CpuAccuracy::Auto, CpuAccuracy::Auto, CpuAccuracy::Paranoid, "cpu_accuracy", Cpu),
            cpu_debug_mode: SwitchableSetting::new(false, "cpu_debug_mode", CpuDebug),

            cpuopt_page_tables: Setting::new(true, "cpuopt_page_tables", CpuDebug),
            cpuopt_block_linking: Setting::new(true, "cpuopt_block_linking", CpuDebug),
            cpuopt_return_stack_buffer: Setting::new(true, "cpuopt_return_stack_buffer", CpuDebug),
            cpuopt_fast_dispatcher: Setting::new(true, "cpuopt_fast_dispatcher", CpuDebug),
            cpuopt_context_elimination: Setting::new(true, "cpuopt_context_elimination", CpuDebug),
            cpuopt_const_prop: Setting::new(true, "cpuopt_const_prop", CpuDebug),
            cpuopt_misc_ir: Setting::new(true, "cpuopt_misc_ir", CpuDebug),
            cpuopt_reduce_misalign_checks: Setting::new(true, "cpuopt_reduce_misalign_checks", CpuDebug),
            cpuopt_fastmem: SwitchableSetting::new(true, "cpuopt_fastmem", CpuDebug),
            cpuopt_fastmem_exclusives: SwitchableSetting::new(true, "cpuopt_fastmem_exclusives", CpuDebug),
            cpuopt_recompile_exclusives: Setting::new(true, "cpuopt_recompile_exclusives", CpuDebug),
            cpuopt_ignore_memory_aborts: Setting::new(true, "cpuopt_ignore_memory_aborts", CpuDebug),

            cpuopt_unsafe_unfuse_fma: SwitchableSetting::new(true, "cpuopt_unsafe_unfuse_fma", CpuUnsafe),
            cpuopt_unsafe_reduce_fp_error: SwitchableSetting::new(true, "cpuopt_unsafe_reduce_fp_error", CpuUnsafe),
            cpuopt_unsafe_ignore_standard_fpcr: SwitchableSetting::new(true, "cpuopt_unsafe_ignore_standard_fpcr", CpuUnsafe),
            cpuopt_unsafe_inaccurate_nan: SwitchableSetting::new(true, "cpuopt_unsafe_inaccurate_nan", CpuUnsafe),
            cpuopt_unsafe_fastmem_check: SwitchableSetting::new(true, "cpuopt_unsafe_fastmem_check", CpuUnsafe),
            cpuopt_unsafe_ignore_global_monitor: SwitchableSetting::new(true, "cpuopt_unsafe_ignore_global_monitor", CpuUnsafe),

            // Renderer
            renderer_backend: SwitchableSetting::ranged(RendererBackend::Vulkan, RendererBackend::OpenGL, RendererBackend::Null, "backend", Renderer),
            shader_backend: SwitchableSetting::ranged_with_options(ShaderBackend::Glsl, ShaderBackend::Glsl, ShaderBackend::SpirV, "shader_backend", Renderer, Specialization::RUNTIME_LIST, true, false),
            vulkan_device: SwitchableSetting::with_options(0, "vulkan_device", Renderer, Specialization::RUNTIME_LIST, true, false),

            use_disk_shader_cache: SwitchableSetting::new(true, "use_disk_shader_cache", Renderer),
            use_asynchronous_gpu_emulation: SwitchableSetting::new(true, "use_asynchronous_gpu_emulation", Renderer),
            accelerate_astc: SwitchableSetting::ranged(AstcDecodeMode::Gpu, AstcDecodeMode::Cpu, AstcDecodeMode::CpuAsynchronous, "accelerate_astc", Renderer),
            vsync_mode: SwitchableSetting::ranged_with_options(VSyncMode::Fifo, VSyncMode::Immediate, VSyncMode::FifoRelaxed, "use_vsync", Renderer, Specialization::RUNTIME_LIST, true, true),
            nvdec_emulation: SwitchableSetting::new(NvdecEmulation::Gpu, "nvdec_emulation", Renderer),
            fullscreen_mode: SwitchableSetting::ranged_with_options(FullscreenMode::Exclusive, FullscreenMode::Borderless, FullscreenMode::Exclusive, "fullscreen_mode", Renderer, Specialization::DEFAULT, true, true),
            aspect_ratio: SwitchableSetting::ranged_with_options(AspectRatio::R16_9, AspectRatio::R16_9, AspectRatio::Stretch, "aspect_ratio", Renderer, Specialization::DEFAULT, true, true),

            resolution_info: ResolutionScalingInfo::default(),
            resolution_setup: SwitchableSetting::new(ResolutionSetup::Res1X, "resolution_setup", Renderer),
            scaling_filter: SwitchableSetting::with_options(ScalingFilter::Bilinear, "scaling_filter", Renderer, Specialization::DEFAULT, true, true),
            anti_aliasing: SwitchableSetting::with_options(AntiAliasing::None, "anti_aliasing", Renderer, Specialization::DEFAULT, true, true),
            fsr_sharpening_slider: SwitchableSetting::ranged_with_options(25, 0, 200, "fsr_sharpening_slider", Renderer, Specialization::SCALAR | Specialization::PERCENTAGE, true, true),

            bg_red: SwitchableSetting::with_options(0, "bg_red", Renderer, Specialization::DEFAULT, true, true),
            bg_green: SwitchableSetting::with_options(0, "bg_green", Renderer, Specialization::DEFAULT, true, true),
            bg_blue: SwitchableSetting::with_options(0, "bg_blue", Renderer, Specialization::DEFAULT, true, true),

            // Renderer Advanced
            gpu_accuracy: SwitchableSetting::ranged_with_options(GpuAccuracy::High, GpuAccuracy::Normal, GpuAccuracy::Extreme, "gpu_accuracy", RendererAdvanced, Specialization::DEFAULT, true, true),
            current_gpu_accuracy: GpuAccuracy::High,
            max_anisotropy: SwitchableSetting::ranged(AnisotropyMode::Automatic, AnisotropyMode::Automatic, AnisotropyMode::X16, "max_anisotropy", RendererAdvanced),
            astc_recompression: SwitchableSetting::ranged(AstcRecompression::Uncompressed, AstcRecompression::Uncompressed, AstcRecompression::Bc3, "astc_recompression", RendererAdvanced),
            vram_usage_mode: SwitchableSetting::ranged(VramUsageMode::Conservative, VramUsageMode::Conservative, VramUsageMode::Aggressive, "vram_usage_mode", RendererAdvanced),
            async_presentation: SwitchableSetting::new(false, "async_presentation", RendererAdvanced),
            renderer_force_max_clock: SwitchableSetting::new(false, "force_max_clock", RendererAdvanced),
            use_reactive_flushing: SwitchableSetting::new(true, "use_reactive_flushing", RendererAdvanced),
            use_asynchronous_shaders: SwitchableSetting::new(false, "use_asynchronous_shaders", RendererAdvanced),
            use_fast_gpu_time: SwitchableSetting::with_options(true, "use_fast_gpu_time", RendererAdvanced, Specialization::DEFAULT, true, true),
            use_vulkan_driver_pipeline_cache: SwitchableSetting::with_options(true, "use_vulkan_driver_pipeline_cache", RendererAdvanced, Specialization::DEFAULT, true, true),
            enable_compute_pipelines: SwitchableSetting::new(false, "enable_compute_pipelines", RendererAdvanced),
            use_video_framerate: SwitchableSetting::new(false, "use_video_framerate", RendererAdvanced),
            barrier_feedback_loops: SwitchableSetting::new(true, "barrier_feedback_loops", RendererAdvanced),

            // Renderer Debug
            renderer_debug: Setting::new(false, "debug", RendererDebug),
            renderer_shader_feedback: Setting::new(false, "shader_feedback", RendererDebug),
            enable_nsight_aftermath: Setting::new(false, "nsight_aftermath", RendererDebug),
            disable_shader_loop_safety_checks: Setting::new(false, "disable_shader_loop_safety_checks", RendererDebug),
            enable_renderdoc_hotkey: Setting::new(false, "renderdoc_hotkey", RendererDebug),
            disable_buffer_reorder: Setting::new(false, "disable_buffer_reorder", RendererDebug),

            // System
            language_index: SwitchableSetting::ranged(Language::EnglishAmerican, Language::Japanese, Language::PortugueseBrazilian, "language_index", System),
            region_index: SwitchableSetting::ranged(Region::Usa, Region::Japan, Region::Taiwan, "region_index", System),
            time_zone_index: SwitchableSetting::ranged(TimeZone::Auto, TimeZone::Auto, TimeZone::Zulu, "time_zone_index", System),
            custom_rtc_enabled: SwitchableSetting::with_options(false, "custom_rtc_enabled", System, Specialization::PAIRED, true, true),
            custom_rtc: SwitchableSetting::with_options(0i64, "custom_rtc", System, Specialization::TIME, false, true),
            custom_rtc_offset: SwitchableSetting::ranged_with_options(0i64, i32::MIN as i64, i32::MAX as i64, "custom_rtc_offset", System, Specialization::COUNTABLE, true, true),
            rng_seed_enabled: SwitchableSetting::with_options(false, "rng_seed_enabled", System, Specialization::PAIRED, true, true),
            rng_seed: SwitchableSetting::with_options(0u32, "rng_seed", System, Specialization::HEX, true, true),
            device_name: Setting::with_options("yuzu".to_string(), "device_name", System, Specialization::DEFAULT, true, true),
            current_user: Setting::new(0i32, "current_user", System),
            use_docked_mode: SwitchableSetting::with_options(ConsoleMode::Docked, "use_docked_mode", System, Specialization::RADIO, true, true),

            // Linux
            enable_gamemode: SwitchableSetting::new(true, "enable_gamemode", Linux),

            // Controls
            players: InputSetting::new(),

            enable_raw_input: Setting::with_options(false, "enable_raw_input", Controls, Specialization::DEFAULT, false, false),
            controller_navigation: Setting::new(true, "controller_navigation", Controls),
            enable_joycon_driver: Setting::new(true, "enable_joycon_driver", Controls),
            enable_procon_driver: Setting::new(false, "enable_procon_driver", Controls),

            vibration_enabled: SwitchableSetting::new(true, "vibration_enabled", Controls),
            enable_accurate_vibrations: SwitchableSetting::new(false, "enable_accurate_vibrations", Controls),
            motion_enabled: SwitchableSetting::new(true, "motion_enabled", Controls),

            udp_input_servers: Setting::new("127.0.0.1:26760".to_string(), "udp_input_servers", Controls),
            enable_udp_controller: Setting::new(false, "enable_udp_controller", Controls),

            pause_tas_on_load: Setting::new(true, "pause_tas_on_load", Controls),
            tas_enable: Setting::new(false, "tas_enable", Controls),
            tas_loop: Setting::new(false, "tas_loop", Controls),

            mouse_panning: Setting::with_options(false, "mouse_panning", Controls, Specialization::DEFAULT, false, false),
            mouse_panning_sensitivity: Setting::ranged(50, 1, 100, "mouse_panning_sensitivity", Controls),
            mouse_enabled: Setting::new(false, "mouse_enabled", Controls),
            mouse_panning_x_sensitivity: Setting::ranged(50, 1, 100, "mouse_panning_x_sensitivity", Controls),
            mouse_panning_y_sensitivity: Setting::ranged(50, 1, 100, "mouse_panning_y_sensitivity", Controls),
            mouse_panning_deadzone_counterweight: Setting::ranged(20, 0, 100, "mouse_panning_deadzone_counterweight", Controls),
            mouse_panning_decay_strength: Setting::ranged(18, 0, 100, "mouse_panning_decay_strength", Controls),
            mouse_panning_min_decay: Setting::ranged(6, 0, 100, "mouse_panning_min_decay", Controls),

            emulate_analog_keyboard: Setting::new(false, "emulate_analog_keyboard", Controls),
            keyboard_enabled: Setting::new(false, "keyboard_enabled", Controls),

            debug_pad_enabled: Setting::new(false, "debug_pad_enabled", Controls),
            debug_pad_buttons: Default::default(),
            debug_pad_analogs: Default::default(),

            touchscreen: TouchscreenInput::default(),
            touch_device: Setting::new("min_x:100,min_y:50,max_x:1800,max_y:850".to_string(), "touch_device", Controls),
            touch_from_button_map_index: Setting::new(0, "touch_from_button_map", Controls),
            touch_from_button_maps: Vec::new(),

            enable_ring_controller: Setting::new(true, "enable_ring_controller", Controls),
            ringcon_analogs: String::new(),

            enable_ir_sensor: Setting::new(false, "enable_ir_sensor", Controls),
            ir_sensor_device: Setting::new("auto".to_string(), "ir_sensor_device", Controls),

            random_amiibo_id: Setting::new(false, "random_amiibo_id", Controls),

            // Data Storage
            use_virtual_sd: Setting::new(true, "use_virtual_sd", DataStorage),
            gamecard_inserted: Setting::new(false, "gamecard_inserted", DataStorage),
            gamecard_current_game: Setting::new(false, "gamecard_current_game", DataStorage),
            gamecard_path: Setting::new(String::new(), "gamecard_path", DataStorage),

            // Debugging
            record_frame_times: false,
            use_gdbstub: Setting::new(false, "use_gdbstub", Debugging),
            gdbstub_port: Setting::new(6543, "gdbstub_port", Debugging),
            program_args: Setting::new(String::new(), "program_args", Debugging),
            dump_exefs: Setting::new(false, "dump_exefs", Debugging),
            dump_nso: Setting::new(false, "dump_nso", Debugging),
            dump_shaders: Setting::with_options(false, "dump_shaders", DebuggingGraphics, Specialization::DEFAULT, false, false),
            dump_macros: Setting::with_options(false, "dump_macros", DebuggingGraphics, Specialization::DEFAULT, false, false),
            enable_fs_access_log: Setting::new(false, "enable_fs_access_log", Debugging),
            reporting_services: Setting::with_options(false, "reporting_services", Debugging, Specialization::DEFAULT, false, false),
            quest_flag: Setting::new(false, "quest_flag", Debugging),
            disable_macro_jit: Setting::new(false, "disable_macro_jit", DebuggingGraphics),
            disable_macro_hle: Setting::new(false, "disable_macro_hle", DebuggingGraphics),
            extended_logging: Setting::with_options(false, "extended_logging", Debugging, Specialization::DEFAULT, false, false),
            use_debug_asserts: Setting::new(false, "use_debug_asserts", Debugging),
            use_auto_stub: Setting::with_options(false, "use_auto_stub", Debugging, Specialization::DEFAULT, false, false),
            enable_all_controllers: Setting::new(false, "enable_all_controllers", Debugging),
            perform_vulkan_check: Setting::new(true, "perform_vulkan_check", Debugging),

            // Miscellaneous
            log_filter: Setting::new("*:Info".to_string(), "log_filter", Miscellaneous),
            use_dev_keys: Setting::new(false, "use_dev_keys", Miscellaneous),

            // Network
            network_interface: Setting::new(String::new(), "network_interface", Network),

            // WebService
            enable_telemetry: Setting::new(true, "enable_telemetry", WebService),
            web_api_url: Setting::new("https://api.yuzu-emu.org".to_string(), "web_api_url", WebService),
            yuzu_username: Setting::new(String::new(), "yuzu_username", WebService),
            yuzu_token: Setting::new(String::new(), "yuzu_token", WebService),

            // Add-Ons
            disabled_addons: HashMap::new(),

            // Extra
            title_id: 0,
            keys_dir: None,
            games_dir: None,
        }
    }
}

// ── Free functions matching C++ Settings:: namespace ────────────────────────

/// Update `current_gpu_accuracy` from the switchable setting.
pub fn update_gpu_accuracy(values: &mut Values) {
    values.current_gpu_accuracy = *values.gpu_accuracy.get_value();
}

/// Returns true if GPU accuracy is Extreme.
pub fn is_gpu_level_extreme(values: &Values) -> bool {
    values.current_gpu_accuracy == GpuAccuracy::Extreme
}

/// Returns true if GPU accuracy is High or Extreme.
pub fn is_gpu_level_high(values: &Values) -> bool {
    values.current_gpu_accuracy == GpuAccuracy::Extreme
        || values.current_gpu_accuracy == GpuAccuracy::High
}

/// Returns true if fastmem is effectively enabled.
pub fn is_fastmem_enabled(values: &Values) -> bool {
    if *values.cpu_debug_mode.get_value() {
        *values.cpuopt_fastmem.get_value()
    } else {
        true
    }
}

static IS_NCE_ENABLED: AtomicBool = AtomicBool::new(false);

/// Configure NCE state based on CPU backend selection and address space.
pub fn set_nce_enabled(values: &Values, is_39bit: bool) {
    let is_nce_selected = *values.cpu_backend.get_value() == CpuBackend::Nce;
    if is_nce_selected && !is_fastmem_enabled(values) {
        warn!(
            "Fastmem is required to natively execute code in a performant manner, \
             falling back to Dynarmic"
        );
    }
    if is_nce_selected && !is_39bit {
        warn!(
            "Program does not utilize 39-bit address space, unable to natively execute code"
        );
    }
    IS_NCE_ENABLED.store(
        is_fastmem_enabled(values) && is_nce_selected && is_39bit,
        Ordering::Relaxed,
    );
}

/// Returns true if NCE (native code execution) is enabled.
pub fn is_nce_enabled() -> bool {
    IS_NCE_ENABLED.load(Ordering::Relaxed)
}

/// Returns true if the console is in docked mode.
pub fn is_docked_mode(values: &Values) -> bool {
    *values.use_docked_mode.get_value() == ConsoleMode::Docked
}

/// Returns the effective audio volume as a float (0.0 to ~2.0).
pub fn volume(values: &Values) -> f32 {
    if *values.audio_muted.get_value() {
        return 0.0;
    }
    *values.volume.get_value() as f32 / *values.volume.get_default() as f32
}

/// Translate a `ResolutionSetup` into scaling info.
pub fn translate_resolution_info(setup: ResolutionSetup, info: &mut ResolutionScalingInfo) {
    info.downscale = false;
    match setup {
        ResolutionSetup::Res1_2X => {
            info.up_scale = 1;
            info.down_shift = 1;
            info.downscale = true;
        }
        ResolutionSetup::Res3_4X => {
            info.up_scale = 3;
            info.down_shift = 2;
            info.downscale = true;
        }
        ResolutionSetup::Res1X => {
            info.up_scale = 1;
            info.down_shift = 0;
        }
        ResolutionSetup::Res3_2X => {
            info.up_scale = 3;
            info.down_shift = 1;
        }
        ResolutionSetup::Res2X => {
            info.up_scale = 2;
            info.down_shift = 0;
        }
        ResolutionSetup::Res3X => {
            info.up_scale = 3;
            info.down_shift = 0;
        }
        ResolutionSetup::Res4X => {
            info.up_scale = 4;
            info.down_shift = 0;
        }
        ResolutionSetup::Res5X => {
            info.up_scale = 5;
            info.down_shift = 0;
        }
        ResolutionSetup::Res6X => {
            info.up_scale = 6;
            info.down_shift = 0;
        }
        ResolutionSetup::Res7X => {
            info.up_scale = 7;
            info.down_shift = 0;
        }
        ResolutionSetup::Res8X => {
            info.up_scale = 8;
            info.down_shift = 0;
        }
    }
    info.up_factor = info.up_scale as f32 / (1u32 << info.down_shift) as f32;
    info.down_factor = (1u32 << info.down_shift) as f32 / info.up_scale as f32;
    info.active = info.up_scale != 1 || info.down_shift != 0;
}

/// Update the global resolution scaling info from the current resolution_setup setting.
pub fn update_rescaling_info(values: &mut Values) {
    let setup = *values.resolution_setup.get_value();
    translate_resolution_info(setup, &mut values.resolution_info);
}

/// Restore all switchable settings to their global state.
/// Should be called when a game is not running.
pub fn restore_global_state(values: &mut Values, is_powered_on: bool) {
    if is_powered_on {
        return;
    }
    values.sink_id.set_global(true);
    values.audio_output_device_id.set_global(true);
    values.audio_input_device_id.set_global(true);
    values.sound_index.set_global(true);
    values.volume.set_global(true);
    values.use_multi_core.set_global(true);
    values.memory_layout_mode.set_global(true);
    values.use_speed_limit.set_global(true);
    values.speed_limit.set_global(true);
    values.cpu_backend.set_global(true);
    values.cpu_accuracy.set_global(true);
    values.cpu_debug_mode.set_global(true);
    values.cpuopt_fastmem.set_global(true);
    values.cpuopt_fastmem_exclusives.set_global(true);
    values.cpuopt_unsafe_unfuse_fma.set_global(true);
    values.cpuopt_unsafe_reduce_fp_error.set_global(true);
    values.cpuopt_unsafe_ignore_standard_fpcr.set_global(true);
    values.cpuopt_unsafe_inaccurate_nan.set_global(true);
    values.cpuopt_unsafe_fastmem_check.set_global(true);
    values.cpuopt_unsafe_ignore_global_monitor.set_global(true);
    values.renderer_backend.set_global(true);
    values.shader_backend.set_global(true);
    values.vulkan_device.set_global(true);
    values.use_disk_shader_cache.set_global(true);
    values.use_asynchronous_gpu_emulation.set_global(true);
    values.accelerate_astc.set_global(true);
    values.vsync_mode.set_global(true);
    values.nvdec_emulation.set_global(true);
    values.fullscreen_mode.set_global(true);
    values.aspect_ratio.set_global(true);
    values.resolution_setup.set_global(true);
    values.scaling_filter.set_global(true);
    values.anti_aliasing.set_global(true);
    values.fsr_sharpening_slider.set_global(true);
    values.bg_red.set_global(true);
    values.bg_green.set_global(true);
    values.bg_blue.set_global(true);
    values.gpu_accuracy.set_global(true);
    values.max_anisotropy.set_global(true);
    values.astc_recompression.set_global(true);
    values.vram_usage_mode.set_global(true);
    values.async_presentation.set_global(true);
    values.renderer_force_max_clock.set_global(true);
    values.use_reactive_flushing.set_global(true);
    values.use_asynchronous_shaders.set_global(true);
    values.use_fast_gpu_time.set_global(true);
    values.use_vulkan_driver_pipeline_cache.set_global(true);
    values.enable_compute_pipelines.set_global(true);
    values.use_video_framerate.set_global(true);
    values.barrier_feedback_loops.set_global(true);
    values.language_index.set_global(true);
    values.region_index.set_global(true);
    values.time_zone_index.set_global(true);
    values.custom_rtc_enabled.set_global(true);
    values.custom_rtc.set_global(true);
    values.custom_rtc_offset.set_global(true);
    values.rng_seed_enabled.set_global(true);
    values.rng_seed.set_global(true);
    values.use_docked_mode.set_global(true);
    values.enable_gamemode.set_global(true);
    values.vibration_enabled.set_global(true);
    values.enable_accurate_vibrations.set_global(true);
    values.motion_enabled.set_global(true);
}

static CONFIGURING_GLOBAL: AtomicBool = AtomicBool::new(true);

/// Returns true if the frontend is currently configuring global (not per-game) settings.
pub fn is_configuring_global() -> bool {
    CONFIGURING_GLOBAL.load(Ordering::Relaxed)
}

/// Set the global configuration state.
pub fn set_configuring_global(is_global: bool) {
    CONFIGURING_GLOBAL.store(is_global, Ordering::Relaxed);
}

/// Log all settings. Matches C++ `Settings::LogSettings()`.
pub fn log_settings(values: &Values) {
    info!("ruzu Configuration:");
    info!("  use_multi_core: {}", values.use_multi_core.get_value());
    info!("  cpu_accuracy: {:?}", values.cpu_accuracy.get_value());
    info!("  cpu_backend: {:?}", values.cpu_backend.get_value());
    info!("  renderer_backend: {:?}", values.renderer_backend.get_value());
    info!("  shader_backend: {:?}", values.shader_backend.get_value());
    info!("  vulkan_device: {}", values.vulkan_device.get_value());
    info!("  gpu_accuracy: {:?}", values.gpu_accuracy.get_value());
    info!("  resolution_setup: {:?}", values.resolution_setup.get_value());
    info!("  vsync_mode: {:?}", values.vsync_mode.get_value());
    info!("  language_index: {:?}", values.language_index.get_value());
    info!("  region_index: {:?}", values.region_index.get_value());
    info!("  use_docked_mode: {:?}", values.use_docked_mode.get_value());
    info!("  volume: {}", values.volume.get_value());
}

// ── Backward-compatibility type alias ───────────────────────────────────────

/// The old `Settings` struct name -- now an alias to `Values`.
pub type Settings = Values;

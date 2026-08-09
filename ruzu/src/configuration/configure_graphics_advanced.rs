// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_graphics_advanced.cpp`
// (`ConfigureGraphicsAdvanced`), whose widget tree lives in
// `configure_graphics_advanced.ui`.
//
// A single "Advanced Graphics Settings" group: four combos followed by the
// boolean toggles. Upstream's `ExposeComputeOption()` additionally reveals the
// "Enable compute pipelines" check box when the selected Vulkan driver needs it;
// the row is built here but stays hidden until that call, matching upstream's
// default state.

use gtk::prelude::*;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// The page plus upstream's `ExposeComputeOption` callback. `ConfigurePerGame`
/// passes the callback to `ConfigureGraphics`, preserving the same ownership
/// and construction order as the C++ dialog.
pub struct BuildResult {
    pub page: Page,
    pub expose_compute_option: Box<dyn Fn()>,
}

/// Build the Graphics "Advanced" tab — upstream `ConfigureGraphicsAdvanced`.
pub fn page() -> BuildResult {
    let (scroller, column) = w::page();

    let (group, content) = w::group("Advanced Graphics Settings");

    let accuracy_value = *common::settings::values().gpu_accuracy.get_value();
    let (accuracy_row, accuracy) = w::combo_row(
        "Accuracy Level:",
        &tr::labels(tr::GPU_ACCURACY),
        tr::index_of(tr::GPU_ACCURACY, &accuracy_value),
    );
    content.append(&accuracy_row);

    let fence_behavior_value = *common::settings::values().gpu_fence_behavior.get_value();
    let (fence_behavior_row, fence_behavior) = w::combo_row(
        "GPU Fence Behavior:",
        &tr::labels(tr::GPU_FENCE_BEHAVIOR),
        tr::index_of(tr::GPU_FENCE_BEHAVIOR, &fence_behavior_value),
    );
    content.append(&fence_behavior_row);

    let aniso_value = *common::settings::values().max_anisotropy.get_value();
    let (aniso_row, aniso) = w::combo_row(
        "Anisotropic Filtering:",
        &tr::labels(tr::ANISOTROPY_MODE),
        tr::index_of(tr::ANISOTROPY_MODE, &aniso_value),
    );
    content.append(&aniso_row);

    let recompression_value = *common::settings::values().astc_recompression.get_value();
    let (recompression_row, recompression) = w::combo_row(
        "ASTC Recompression Method:",
        &tr::labels(tr::ASTC_RECOMPRESSION),
        tr::index_of(tr::ASTC_RECOMPRESSION, &recompression_value),
    );
    content.append(&recompression_row);

    let vram_value = *common::settings::values().vram_usage_mode.get_value();
    let (vram_row, vram) = w::combo_row(
        "VRAM Usage Mode:",
        &tr::labels(tr::VRAM_USAGE_MODE),
        tr::index_of(tr::VRAM_USAGE_MODE, &vram_value),
    );
    content.append(&vram_row);

    let async_presentation = w::check_row(
        "Enable asynchronous presentation (Vulkan only)",
        *common::settings::values().async_presentation.get_value(),
    );
    let force_max_clock = w::check_row(
        "Force maximum clocks (Vulkan only)",
        *common::settings::values()
            .renderer_force_max_clock
            .get_value(),
    );
    let reactive_flushing = w::check_row(
        "Enable Reactive Flushing",
        *common::settings::values().use_reactive_flushing.get_value(),
    );
    let async_shaders = w::check_row(
        "Use asynchronous shader building (Hack)",
        *common::settings::values()
            .use_asynchronous_shaders
            .get_value(),
    );
    let fast_gpu_time = w::check_row(
        "Use Fast GPU Time (Hack)",
        *common::settings::values().use_fast_gpu_time.get_value(),
    );
    let vulkan_pipeline_cache = w::check_row(
        "Use Vulkan pipeline cache",
        *common::settings::values()
            .use_vulkan_driver_pipeline_cache
            .get_value(),
    );
    let video_framerate = w::check_row(
        "Sync to framerate of video playback",
        *common::settings::values().use_video_framerate.get_value(),
    );
    let barrier_feedback_loops = w::check_row(
        "Barrier feedback loops",
        *common::settings::values()
            .barrier_feedback_loops
            .get_value(),
    );
    for check in [
        &async_presentation,
        &force_max_clock,
        &reactive_flushing,
        &async_shaders,
        &fast_gpu_time,
        &vulkan_pipeline_cache,
        &video_framerate,
        &barrier_feedback_loops,
    ] {
        content.append(check);
    }

    // Upstream builds this row but leaves it hidden until
    // `ExposeComputeOption()` is called by `ConfigureGraphics` for the drivers
    // that need it.
    let compute_pipelines = w::check_row(
        "Enable compute pipelines (Intel Vulkan only)",
        *common::settings::values()
            .enable_compute_pipelines
            .get_value(),
    );
    compute_pipelines.set_visible(false);
    content.append(&compute_pipelines);

    column.append(&group);

    let expose_compute_pipelines = compute_pipelines.clone();
    let page = Page::new("Adv. Graphics", scroller, move || {
        let accuracy_value = tr::value_at(tr::GPU_ACCURACY, accuracy.selected());
        let fence_behavior_value = tr::value_at(tr::GPU_FENCE_BEHAVIOR, fence_behavior.selected());
        let aniso_value = tr::value_at(tr::ANISOTROPY_MODE, aniso.selected());
        let recompression_value = tr::value_at(tr::ASTC_RECOMPRESSION, recompression.selected());
        let vram_value = tr::value_at(tr::VRAM_USAGE_MODE, vram.selected());
        let async_present = async_presentation.is_active();
        let max_clock = force_max_clock.is_active();
        let reactive = reactive_flushing.is_active();
        let shaders = async_shaders.is_active();
        let fast_gpu = fast_gpu_time.is_active();
        let pipeline_cache = vulkan_pipeline_cache.is_active();
        let framerate = video_framerate.is_active();
        let barriers = barrier_feedback_loops.is_active();
        let compute = compute_pipelines.is_active();

        let mut values = common::settings::values_mut();
        values.gpu_accuracy.set_value(accuracy_value);
        values.gpu_fence_behavior.set_value(fence_behavior_value);
        values.max_anisotropy.set_value(aniso_value);
        values.astc_recompression.set_value(recompression_value);
        values.vram_usage_mode.set_value(vram_value);
        values.async_presentation.set_value(async_present);
        values.renderer_force_max_clock.set_value(max_clock);
        values.use_reactive_flushing.set_value(reactive);
        values.use_asynchronous_shaders.set_value(shaders);
        values.use_fast_gpu_time.set_value(fast_gpu);
        values
            .use_vulkan_driver_pipeline_cache
            .set_value(pipeline_cache);
        values.use_video_framerate.set_value(framerate);
        values.barrier_feedback_loops.set_value(barriers);
        values.enable_compute_pipelines.set_value(compute);
    });

    BuildResult {
        page,
        expose_compute_option: Box::new(move || expose_compute_pipelines.set_visible(true)),
    }
}

// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/eden/src/yuzu/configuration/configure_graphics_advanced.cpp`
// (`ConfigureGraphicsAdvanced`), whose widget tree lives in
// `configure_graphics_advanced.ui`.
//
// A single "Advanced Graphics Settings" group populated in upstream setting-id
// order. Upstream's `ExposeComputeOption()` additionally reveals the
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
        "GPU Mode:",
        &tr::labels(tr::GPU_ACCURACY),
        tr::index_of(tr::GPU_ACCURACY, &accuracy_value),
    );
    content.append(&accuracy_row);

    let dma_value = *common::settings::values().dma_accuracy.get_value();
    let (dma_row, dma) = w::combo_row(
        "DMA Accuracy:",
        &tr::labels(tr::DMA_ACCURACY),
        tr::index_of(tr::DMA_ACCURACY, &dma_value),
    );
    content.append(&dma_row);

    let fence_behavior_value = *common::settings::values().gpu_fence_behavior.get_value();
    let (fence_behavior_row, fence_behavior) = w::combo_row(
        "GPU Fence Behavior:",
        &tr::labels(tr::GPU_FENCE_BEHAVIOR),
        tr::index_of(tr::GPU_FENCE_BEHAVIOR, &fence_behavior_value),
    );
    content.append(&fence_behavior_row);

    let vram_value = *common::settings::values().vram_usage_mode.get_value();
    let (vram_row, vram) = w::combo_row(
        "VRAM Usage Mode:",
        &tr::labels(tr::VRAM_USAGE_MODE),
        tr::index_of(tr::VRAM_USAGE_MODE, &vram_value),
    );
    content.append(&vram_row);

    let nvdec_value = *common::settings::values().nvdec_emulation.get_value();
    let (nvdec_row, nvdec) = w::combo_row(
        "NVDEC emulation:",
        &tr::labels(tr::NVDEC_EMULATION),
        tr::index_of(tr::NVDEC_EMULATION, &nvdec_value),
    );
    content.append(&nvdec_row);

    let aniso_value = *common::settings::values().max_anisotropy.get_value();
    let (aniso_row, aniso) = w::combo_row(
        "Anisotropic Filtering:",
        &tr::labels(tr::ANISOTROPY_MODE),
        tr::index_of(tr::ANISOTROPY_MODE, &aniso_value),
    );
    content.append(&aniso_row);

    let astc_value = *common::settings::values().accelerate_astc.get_value();
    let (astc_row, astc) = w::combo_row(
        "ASTC Decoding Method:",
        &tr::labels(tr::ASTC_DECODE_MODE),
        tr::index_of(tr::ASTC_DECODE_MODE, &astc_value),
    );
    content.append(&astc_row);

    let frame_pacing_value = *common::settings::values().frame_pacing_mode.get_value();
    let (frame_pacing_row, frame_pacing) = w::combo_row(
        "Frame Pacing Mode (Vulkan only)",
        &tr::labels(tr::FRAME_PACING_MODE),
        tr::index_of(tr::FRAME_PACING_MODE, &frame_pacing_value),
    );
    content.append(&frame_pacing_row);

    let recompression_value = *common::settings::values().astc_recompression.get_value();
    let (recompression_row, recompression) = w::combo_row(
        "ASTC Recompression Method:",
        &tr::labels(tr::ASTC_RECOMPRESSION),
        tr::index_of(tr::ASTC_RECOMPRESSION, &recompression_value),
    );
    content.append(&recompression_row);

    let sync_memory = w::check_row(
        "Sync Memory Operations",
        *common::settings::values()
            .sync_memory_operations
            .get_value(),
    );
    content.append(&sync_memory);

    let force_max_clock = w::check_row(
        "Force maximum clocks (Vulkan only)",
        *common::settings::values()
            .renderer_force_max_clock
            .get_value(),
    );
    content.append(&force_max_clock);

    let disk_pipeline_cache = w::check_row(
        "Use persistent pipeline cache",
        *common::settings::values().use_disk_shader_cache.get_value(),
    );
    content.append(&disk_pipeline_cache);

    let vulkan_pipeline_cache = w::check_row(
        "Use Vulkan pipeline cache",
        *common::settings::values()
            .use_vulkan_driver_pipeline_cache
            .get_value(),
    );
    content.append(&vulkan_pipeline_cache);

    // Upstream builds this row in setting-id order but leaves it hidden until
    // `ExposeComputeOption()` is called by `ConfigureGraphics` for a driver
    // that reports broken compute support.
    let compute_pipelines = w::check_row(
        "Enable compute pipelines (Intel Vulkan only)",
        *common::settings::values()
            .enable_compute_pipelines
            .get_value(),
    );
    compute_pipelines.set_visible(false);
    content.append(&compute_pipelines);

    let video_framerate = w::check_row(
        "Sync to framerate of video playback",
        *common::settings::values().use_video_framerate.get_value(),
    );
    content.append(&video_framerate);

    let reactive_flushing = w::check_row(
        "Enable Reactive Flushing",
        *common::settings::values().use_reactive_flushing.get_value(),
    );
    content.append(&reactive_flushing);

    let barrier_feedback_loops = w::check_row(
        "Barrier feedback loops",
        *common::settings::values()
            .barrier_feedback_loops
            .get_value(),
    );
    content.append(&barrier_feedback_loops);

    let buffer_history = w::check_row(
        "Enable buffer history",
        *common::settings::values().enable_buffer_history.get_value(),
    );
    content.append(&buffer_history);

    let gpu_buffer_readback = w::check_row(
        "Enable GPU buffer readback",
        *common::settings::values()
            .enable_gpu_buffer_readback
            .get_value(),
    );
    content.append(&gpu_buffer_readback);

    column.append(&group);

    let expose_compute_pipelines = compute_pipelines.clone();
    let page = Page::new("Advanced", scroller, move || {
        let accuracy_value = tr::value_at(tr::GPU_ACCURACY, accuracy.selected());
        let dma_value = tr::value_at(tr::DMA_ACCURACY, dma.selected());
        let fence_behavior_value = tr::value_at(tr::GPU_FENCE_BEHAVIOR, fence_behavior.selected());
        let vram_value = tr::value_at(tr::VRAM_USAGE_MODE, vram.selected());
        let nvdec_value = tr::value_at(tr::NVDEC_EMULATION, nvdec.selected());
        let aniso_value = tr::value_at(tr::ANISOTROPY_MODE, aniso.selected());
        let astc_value = tr::value_at(tr::ASTC_DECODE_MODE, astc.selected());
        let frame_pacing_value = tr::value_at(tr::FRAME_PACING_MODE, frame_pacing.selected());
        let recompression_value = tr::value_at(tr::ASTC_RECOMPRESSION, recompression.selected());
        let sync_memory_value = sync_memory.is_active();
        let max_clock = force_max_clock.is_active();
        let disk_cache = disk_pipeline_cache.is_active();
        let pipeline_cache = vulkan_pipeline_cache.is_active();
        let compute = compute_pipelines.is_active();
        let framerate = video_framerate.is_active();
        let reactive = reactive_flushing.is_active();
        let barriers = barrier_feedback_loops.is_active();
        let history = buffer_history.is_active();
        let readback = gpu_buffer_readback.is_active();

        let mut values = common::settings::values_mut();
        values.gpu_accuracy.set_value(accuracy_value);
        values.dma_accuracy.set_value(dma_value);
        values.gpu_fence_behavior.set_value(fence_behavior_value);
        values.vram_usage_mode.set_value(vram_value);
        values.nvdec_emulation.set_value(nvdec_value);
        values.max_anisotropy.set_value(aniso_value);
        values.accelerate_astc.set_value(astc_value);
        values.frame_pacing_mode.set_value(frame_pacing_value);
        values.astc_recompression.set_value(recompression_value);
        values.sync_memory_operations.set_value(sync_memory_value);
        values.renderer_force_max_clock.set_value(max_clock);
        values.use_disk_shader_cache.set_value(disk_cache);
        values
            .use_vulkan_driver_pipeline_cache
            .set_value(pipeline_cache);
        values.enable_compute_pipelines.set_value(compute);
        values.use_video_framerate.set_value(framerate);
        values.use_reactive_flushing.set_value(reactive);
        values.barrier_feedback_loops.set_value(barriers);
        values.enable_buffer_history.set_value(history);
        values.enable_gpu_buffer_readback.set_value(readback);
    });

    BuildResult {
        page,
        expose_compute_option: Box::new(move || expose_compute_pipelines.set_visible(true)),
    }
}

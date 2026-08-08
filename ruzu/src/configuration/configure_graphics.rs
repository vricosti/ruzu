// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_graphics.cpp`
// (`ConfigureGraphics`), whose widget tree lives in `configure_graphics.ui`.
//
// Two groups: "API Settings" (backend + shader backend / device) and
// "Graphics Settings" (the render options).
//
// Upstream swaps the second row of "API Settings" depending on the backend:
// OpenGL shows "Shader Backend:", Vulkan shows "Device:", Null shows neither
// (`ConfigureGraphics::UpdateAPILayout`). The VSync combo is likewise rebuilt
// per backend by `PopulateVSyncModeSelection`, because each backend supports a
// different subset of the modes.

use gtk::prelude::*;

use common::settings_enums::RendererBackend;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// Build the Graphics tab — upstream `ConfigureGraphics`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    // --- "API Settings" ---------------------------------------------------
    let (api_group, api) = w::group("API Settings");

    let backend_value = *common::settings::values().renderer_backend.get_value();
    let (backend_row, backend) = w::combo_row(
        "API:",
        &tr::labels(tr::RENDERER_BACKEND),
        tr::index_of(tr::RENDERER_BACKEND, &backend_value),
    );
    api.append(&backend_row);

    let shader_value = *common::settings::values().shader_backend.get_value();
    let (shader_row, shader) = w::combo_row(
        "Shader Backend:",
        &tr::labels(tr::SHADER_BACKEND),
        tr::index_of(tr::SHADER_BACKEND, &shader_value),
    );
    api.append(&shader_row);

    // Vulkan physical-device picker, populated from the upstream-owned
    // `VkDeviceInfo::Record` counterpart.
    let mut device_records = Vec::new();
    crate::vk_device_info::populate_records(&mut device_records);
    let mut device_labels: Vec<String> = device_records
        .iter()
        .map(|record| record.name.clone())
        .collect();
    if device_labels.is_empty() {
        device_labels.push("Device 0".to_string());
    }
    let device_label_refs: Vec<&str> = device_labels.iter().map(String::as_str).collect();
    let selected_device = (*common::settings::values().vulkan_device.get_value()).max(0) as u32;
    let (device_row, device) = w::combo_row("Device:", &device_label_refs, selected_device);
    api.append(&device_row);

    // Only one of the two rows is ever visible, matching `UpdateAPILayout`.
    apply_api_layout(backend_value, &shader_row, &device_row);

    column.append(&api_group);

    // --- "Graphics Settings" ----------------------------------------------
    let (settings_group, settings) = w::group("Graphics Settings");

    let disk_cache = w::check_row(
        "Use disk pipeline cache",
        *common::settings::values().use_disk_shader_cache.get_value(),
    );
    settings.append(&disk_cache);

    let async_gpu = w::check_row(
        "Use asynchronous GPU emulation",
        *common::settings::values()
            .use_asynchronous_gpu_emulation
            .get_value(),
    );
    settings.append(&async_gpu);

    let astc_value = *common::settings::values().accelerate_astc.get_value();
    let (astc_row, astc) = w::combo_row(
        "ASTC Decoding Method:",
        &tr::labels(tr::ASTC_DECODE_MODE),
        tr::index_of(tr::ASTC_DECODE_MODE, &astc_value),
    );
    settings.append(&astc_row);

    let vsync_labels: Vec<&str> = tr::VSYNC_MODE_LABELS.iter().map(|(_, l)| *l).collect();
    let vsync_index = tr::VSYNC_MODE_LABELS
        .iter()
        .position(|(name, _)| {
            *name
                == common::settings::values()
                    .vsync_mode
                    .get_value()
                    .canonicalize()
        })
        .unwrap_or(0) as u32;
    let (vsync_row, vsync) = w::combo_row("VSync Mode:", &vsync_labels, vsync_index);
    settings.append(&vsync_row);

    let nvdec_value = *common::settings::values().nvdec_emulation.get_value();
    let (nvdec_row, nvdec) = w::combo_row(
        "NVDEC emulation:",
        &tr::labels(tr::NVDEC_EMULATION),
        tr::index_of(tr::NVDEC_EMULATION, &nvdec_value),
    );
    settings.append(&nvdec_row);

    let fullscreen_value = *common::settings::values().fullscreen_mode.get_value();
    let (fullscreen_row, fullscreen) = w::combo_row(
        "Fullscreen Mode:",
        &tr::labels(tr::FULLSCREEN_MODE),
        tr::index_of(tr::FULLSCREEN_MODE, &fullscreen_value),
    );
    settings.append(&fullscreen_row);

    let aspect_value = *common::settings::values().aspect_ratio.get_value();
    let (aspect_row, aspect) = w::combo_row(
        "Aspect Ratio:",
        &tr::labels(tr::ASPECT_RATIO),
        tr::index_of(tr::ASPECT_RATIO, &aspect_value),
    );
    settings.append(&aspect_row);

    let resolution_value = *common::settings::values().resolution_setup.get_value();
    let (resolution_row, resolution) = w::combo_row(
        "Resolution:",
        &tr::labels(tr::RESOLUTION_SETUP),
        tr::index_of(tr::RESOLUTION_SETUP, &resolution_value),
    );
    settings.append(&resolution_row);

    let filter_value = *common::settings::values().scaling_filter.get_value();
    let (filter_row, filter) = w::combo_row(
        "Window Adapting Filter:",
        &tr::labels(tr::SCALING_FILTER),
        tr::index_of(tr::SCALING_FILTER, &filter_value),
    );
    settings.append(&filter_row);

    let aa_value = *common::settings::values().anti_aliasing.get_value();
    let (aa_row, aa) = w::combo_row(
        "Anti-Aliasing Method:",
        &tr::labels(tr::ANTI_ALIASING),
        tr::index_of(tr::ANTI_ALIASING, &aa_value),
    );
    settings.append(&aa_row);

    let sharpness_value = *common::settings::values().fsr_sharpening_slider.get_value();
    let (sharpness_row, sharpness, _) =
        w::percent_slider_row("FSR Sharpness:", sharpness_value as f64, 0.0, 200.0);
    settings.append(&sharpness_row);

    let bg_color = gtk::ColorButton::with_rgba(&background_rgba());
    bg_color.set_halign(gtk::Align::Start);
    settings.append(&w::labeled_row("Background Color:", &bg_color));

    column.append(&settings_group);

    // Reveal the shader-backend / device row that matches the chosen API.
    {
        let shader_row = shader_row.clone();
        let device_row = device_row.clone();
        backend.connect_selected_notify(move |combo| {
            let selected = tr::value_at(tr::RENDERER_BACKEND, combo.selected());
            apply_api_layout(selected, &shader_row, &device_row);
        });
    }

    Page::new("Graphics", scroller, move || {
        let backend_value = tr::value_at(tr::RENDERER_BACKEND, backend.selected());
        let shader_value = tr::value_at(tr::SHADER_BACKEND, shader.selected());
        let device_index = device.selected() as i32;
        let disk = disk_cache.is_active();
        let async_value = async_gpu.is_active();
        let astc_value = tr::value_at(tr::ASTC_DECODE_MODE, astc.selected());
        let vsync_name = tr::VSYNC_MODE_LABELS
            .get(vsync.selected() as usize)
            .map(|(name, _)| *name)
            .unwrap_or("Fifo");
        let nvdec_value = tr::value_at(tr::NVDEC_EMULATION, nvdec.selected());
        let fullscreen_value = tr::value_at(tr::FULLSCREEN_MODE, fullscreen.selected());
        let aspect_value = tr::value_at(tr::ASPECT_RATIO, aspect.selected());
        let resolution_value = tr::value_at(tr::RESOLUTION_SETUP, resolution.selected());
        let filter_value = tr::value_at(tr::SCALING_FILTER, filter.selected());
        let aa_value = tr::value_at(tr::ANTI_ALIASING, aa.selected());
        let sharpness_value = sharpness.value() as i32;
        let rgba = bg_color.rgba();

        let mut values = common::settings::values_mut();
        values.renderer_backend.set_value(backend_value);
        values.shader_backend.set_value(shader_value);
        values.vulkan_device.set_value(device_index);
        values.use_disk_shader_cache.set_value(disk);
        values.use_asynchronous_gpu_emulation.set_value(async_value);
        values.accelerate_astc.set_value(astc_value);
        if let Some(mode) = common::settings_enums::VSyncMode::from_string(vsync_name) {
            values.vsync_mode.set_value(mode);
        }
        values.nvdec_emulation.set_value(nvdec_value);
        values.fullscreen_mode.set_value(fullscreen_value);
        values.aspect_ratio.set_value(aspect_value);
        values.resolution_setup.set_value(resolution_value);
        values.scaling_filter.set_value(filter_value);
        values.anti_aliasing.set_value(aa_value);
        values.fsr_sharpening_slider.set_value(sharpness_value);
        values.bg_red.set_value((rgba.red() * 255.0).round() as u8);
        values
            .bg_green
            .set_value((rgba.green() * 255.0).round() as u8);
        values
            .bg_blue
            .set_value((rgba.blue() * 255.0).round() as u8);
    })
}

/// Show the row that belongs to `backend` and hide the other — upstream
/// `ConfigureGraphics::UpdateAPILayout`.
fn apply_api_layout(backend: RendererBackend, shader_row: &gtk::Box, device_row: &gtk::Box) {
    shader_row.set_visible(backend == RendererBackend::OpenGL);
    device_row.set_visible(backend == RendererBackend::Vulkan);
}

/// The configured background colour as a GDK colour.
fn background_rgba() -> gtk::gdk::RGBA {
    let values = common::settings::values();
    gtk::gdk::RGBA::new(
        *values.bg_red.get_value() as f32 / 255.0,
        *values.bg_green.get_value() as f32 / 255.0,
        *values.bg_blue.get_value() as f32 / 255.0,
        1.0,
    )
}

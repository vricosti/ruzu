// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of Eden's
// `yuzu/configuration/configure_graphics_extensions.cpp` and `.ui`.

use gtk::prelude::*;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// Build the `Extras` tab in the same Hacks / Vulkan Extensions order
/// as upstream `ConfigureGraphicsExtensions::Setup`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    let (hacks_group, hacks) = w::group("Hacks");
    let warning = gtk::Label::new(Some(
        "Changing these options from their default may cause issues. Novitii cavete!",
    ));
    warning.set_xalign(0.0);
    hacks.append(&warning);

    let values = common::settings::values();
    let skip_cpu_inner_invalidation = w::check_row(
        "Skip CPU Inner Invalidation",
        *values.skip_cpu_inner_invalidation.get_value(),
    );
    let async_presentation = w::check_row(
        "Enable asynchronous presentation (Vulkan only)",
        *values.async_presentation.get_value(),
    );
    let fix_bloom_effects =
        w::check_row("Fix bloom effects", *values.fix_bloom_effects.get_value());
    let emulate_bgr565 = w::check_row("emulate_bgr565", *values.emulate_bgr565.get_value());
    let rescale_hack = w::check_row(
        "Enable Legacy Rescale Pass",
        *values.rescale_hack.get_value(),
    );
    let asynchronous_shaders = w::check_row(
        "Enable asynchronous shader compilation",
        *values.use_asynchronous_shaders.get_value(),
    );
    for check in [
        &skip_cpu_inner_invalidation,
        &async_presentation,
        &fix_bloom_effects,
        &emulate_bgr565,
        &rescale_hack,
        &asynchronous_shaders,
    ] {
        hacks.append(check);
    }

    let (texture_row, texture_size) = w::combo_row(
        "GPU Unswizzle Max Texture Size",
        &tr::labels(tr::GPU_UNSWIZZLE_SIZE),
        tr::index_of(
            tr::GPU_UNSWIZZLE_SIZE,
            values.gpu_unswizzle_texture_size.get_value(),
        ),
    );
    let (stream_row, stream_size) = w::combo_row(
        "GPU Unswizzle Stream Size",
        &tr::labels(tr::GPU_UNSWIZZLE_STREAM),
        tr::index_of(
            tr::GPU_UNSWIZZLE_STREAM,
            values.gpu_unswizzle_stream_size.get_value(),
        ),
    );
    let (chunk_row, chunk_size) = w::combo_row(
        "GPU Unswizzle Chunk Size",
        &tr::labels(tr::GPU_UNSWIZZLE_CHUNK),
        tr::index_of(
            tr::GPU_UNSWIZZLE_CHUNK,
            values.gpu_unswizzle_chunk_size.get_value(),
        ),
    );
    hacks.append(&texture_row);
    hacks.append(&stream_row);
    hacks.append(&chunk_row);
    let gpu_unswizzle = w::check_row("GPU Unswizzle", *values.gpu_unswizzle_enabled.get_value());
    hacks.append(&gpu_unswizzle);
    column.append(&hacks_group);

    let (extensions_group, extensions) = w::group("Vulkan Extensions");
    let (dynamic_row, dynamic_state) = w::combo_row(
        "Extended Dynamic State",
        &tr::labels(tr::EXTENDED_DYNAMIC_STATE),
        tr::index_of(tr::EXTENDED_DYNAMIC_STATE, values.dyna_state.get_value()),
    );
    #[cfg(target_os = "macos")]
    dynamic_state.set_sensitive(false);
    let (sample_row, sample_shading, _readout) = w::percent_slider_row(
        "Sample Shading",
        *values.sample_shading.get_value() as f64,
        0.0,
        100.0,
    );
    let vertex_input = w::check_row(
        "Vertex Input Dynamic State",
        *values.vertex_input_dynamic_state.get_value(),
    );
    extensions.append(&dynamic_row);
    extensions.append(&sample_row);
    extensions.append(&vertex_input);
    column.append(&extensions_group);
    drop(values);

    Page::new("Extras", scroller, move || {
        let mut values = common::settings::values_mut();
        values
            .skip_cpu_inner_invalidation
            .set_value(skip_cpu_inner_invalidation.is_active());
        values
            .async_presentation
            .set_value(async_presentation.is_active());
        values
            .fix_bloom_effects
            .set_value(fix_bloom_effects.is_active());
        values.emulate_bgr565.set_value(emulate_bgr565.is_active());
        values.rescale_hack.set_value(rescale_hack.is_active());
        values
            .use_asynchronous_shaders
            .set_value(asynchronous_shaders.is_active());
        values.gpu_unswizzle_texture_size.set_value(tr::value_at(
            tr::GPU_UNSWIZZLE_SIZE,
            texture_size.selected(),
        ));
        values.gpu_unswizzle_stream_size.set_value(tr::value_at(
            tr::GPU_UNSWIZZLE_STREAM,
            stream_size.selected(),
        ));
        values
            .gpu_unswizzle_chunk_size
            .set_value(tr::value_at(tr::GPU_UNSWIZZLE_CHUNK, chunk_size.selected()));
        values
            .gpu_unswizzle_enabled
            .set_value(gpu_unswizzle.is_active());
        values.dyna_state.set_value(tr::value_at(
            tr::EXTENDED_DYNAMIC_STATE,
            dynamic_state.selected(),
        ));
        values
            .sample_shading
            .set_value(sample_shading.value() as u32);
        values
            .vertex_input_dynamic_state
            .set_value(vertex_input.is_active());
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn enum_rows_match_upstream_cardinalities() {
        assert_eq!(tr::GPU_UNSWIZZLE_SIZE.len(), 5);
        assert_eq!(tr::GPU_UNSWIZZLE_STREAM.len(), 5);
        assert_eq!(tr::GPU_UNSWIZZLE_CHUNK.len(), 5);
        assert_eq!(tr::EXTENDED_DYNAMIC_STATE.len(), 4);
    }
}

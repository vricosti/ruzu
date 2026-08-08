// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_cpu.cpp`
// (`ConfigureCpu`), whose widget tree lives in `configure_cpu.ui`.
//
// A single "General" group holding the accuracy combo plus the recommendation
// note. Upstream additionally reveals an "Unsafe CPU Optimization Settings"
// group when accuracy is `Unsafe`; that group's rows are the `cpuopt_unsafe_*`
// settings.

use gtk::prelude::*;

use common::settings_enums::CpuAccuracy;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// Build the CPU tab — upstream `ConfigureCpu`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    // --- "General" --------------------------------------------------------
    let (general_group, general) = w::group("General");

    let accuracy_value = *common::settings::values().cpu_accuracy.get_value();
    let (accuracy_row, accuracy) = w::combo_row(
        "Accuracy:",
        &tr::labels(tr::CPU_ACCURACY),
        tr::index_of(tr::CPU_ACCURACY, &accuracy_value),
    );
    general.append(&accuracy_row);

    let note = gtk::Label::new(Some("We recommend setting accuracy to \"Auto\"."));
    note.set_xalign(0.0);
    general.append(&note);

    column.append(&general_group);

    // Upstream compiles and reveals this group only with `HAS_NCE`. Ruzu's NCE
    // backend is available on Linux/AArch64 under the equivalent target cfg.
    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    let backend = {
        let (backend_group, backend_content) = w::group("CPU Backend");
        let backend_value = *common::settings::values().cpu_backend.get_value();
        let (backend_row, backend) = w::combo_row(
            "Backend:",
            &tr::labels(tr::CPU_BACKEND),
            tr::index_of(tr::CPU_BACKEND, &backend_value),
        );
        backend_content.append(&backend_row);
        column.append(&backend_group);
        backend
    };

    // --- "Unsafe CPU Optimization Settings" -------------------------------
    // Upstream shows this group only while accuracy is `Unsafe`
    // (`ConfigureCpu::UpdateGroup`).
    let (unsafe_group, unsafe_content) = w::group("Unsafe CPU Optimization Settings");

    let unsafe_note = gtk::Label::new(Some("These settings reduce accuracy for speed."));
    unsafe_note.set_xalign(0.0);
    unsafe_content.append(&unsafe_note);

    let unfuse_fma = w::check_row(
        "Unfuse FMA (improve performance on CPUs without FMA)",
        *common::settings::values()
            .cpuopt_unsafe_unfuse_fma
            .get_value(),
    );
    let reduce_fp_error = w::check_row(
        "Faster FRSQRTE and FRECPE",
        *common::settings::values()
            .cpuopt_unsafe_reduce_fp_error
            .get_value(),
    );
    let ignore_standard_fpcr = w::check_row(
        "Faster ASIMD instructions (32 bits only)",
        *common::settings::values()
            .cpuopt_unsafe_ignore_standard_fpcr
            .get_value(),
    );
    let inaccurate_nan = w::check_row(
        "Inaccurate NaN handling",
        *common::settings::values()
            .cpuopt_unsafe_inaccurate_nan
            .get_value(),
    );
    let fastmem_check = w::check_row(
        "Disable address space checks",
        *common::settings::values()
            .cpuopt_unsafe_fastmem_check
            .get_value(),
    );
    let ignore_global_monitor = w::check_row(
        "Ignore global monitor",
        *common::settings::values()
            .cpuopt_unsafe_ignore_global_monitor
            .get_value(),
    );
    for check in [
        &unfuse_fma,
        &reduce_fp_error,
        &ignore_standard_fpcr,
        &inaccurate_nan,
        &fastmem_check,
        &ignore_global_monitor,
    ] {
        unsafe_content.append(check);
    }

    unsafe_group.set_visible(accuracy_value == CpuAccuracy::Unsafe);
    column.append(&unsafe_group);

    // Upstream `ConfigureCpu::UpdateGroup`: reveal the unsafe group only for
    // `CpuAccuracy::Unsafe`.
    {
        let unsafe_group = unsafe_group.clone();
        accuracy.connect_selected_notify(move |combo| {
            let selected = tr::value_at(tr::CPU_ACCURACY, combo.selected());
            unsafe_group.set_visible(selected == CpuAccuracy::Unsafe);
        });
    }

    Page::new("CPU", scroller, move || {
        let accuracy_value = tr::value_at(tr::CPU_ACCURACY, accuracy.selected());
        let unfuse = unfuse_fma.is_active();
        let fp_error = reduce_fp_error.is_active();
        let fpcr = ignore_standard_fpcr.is_active();
        let nan = inaccurate_nan.is_active();
        let fastmem = fastmem_check.is_active();
        let monitor = ignore_global_monitor.is_active();

        let mut values = common::settings::values_mut();
        #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
        values
            .cpu_backend
            .set_value(tr::value_at(tr::CPU_BACKEND, backend.selected()));
        values.cpu_accuracy.set_value(accuracy_value);
        values.cpuopt_unsafe_unfuse_fma.set_value(unfuse);
        values.cpuopt_unsafe_reduce_fp_error.set_value(fp_error);
        values.cpuopt_unsafe_ignore_standard_fpcr.set_value(fpcr);
        values.cpuopt_unsafe_inaccurate_nan.set_value(nan);
        values.cpuopt_unsafe_fastmem_check.set_value(fastmem);
        values
            .cpuopt_unsafe_ignore_global_monitor
            .set_value(monitor);
    })
}

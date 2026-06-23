fn main() {
    println!("cargo:rerun-if-changed=src/host1x/ffmpeg/ffmpeg_shim.c");
    println!("cargo:rerun-if-changed=src/textures/bcn_shim.cpp");

    let zuyu_base = std::env::var("ZUYU_DIR").unwrap_or_else(|_| {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/home/vricosti".to_string());
        format!("{}/Dev/emulators/zuyu", home)
    });
    let zuyu_stb_dir = format!("{}/externals/stb", zuyu_base);
    let zuyu_bc_decoder_dir = format!("{}/externals/bc_decoder", zuyu_base);

    println!("cargo:rerun-if-changed={zuyu_stb_dir}/stb_dxt.cpp");
    println!("cargo:rerun-if-changed={zuyu_stb_dir}/stb_dxt.h");
    println!("cargo:rerun-if-changed={zuyu_bc_decoder_dir}/bc_decoder.cpp");
    println!("cargo:rerun-if-changed={zuyu_bc_decoder_dir}/bc_decoder.h");

    let avcodec = pkg_config::Config::new()
        .probe("libavcodec")
        .expect("libavcodec is required for video_core FFmpeg decoding");
    let avutil = pkg_config::Config::new()
        .probe("libavutil")
        .expect("libavutil is required for video_core FFmpeg decoding");

    let mut build = cc::Build::new();
    build.file("src/host1x/ffmpeg/ffmpeg_shim.c");
    build.warnings(false);
    for path in avcodec
        .include_paths
        .iter()
        .chain(avutil.include_paths.iter())
    {
        build.include(path);
    }
    build.compile("ruzu_video_core_ffmpeg_shim");

    let mut bcn_build = cc::Build::new();
    bcn_build.cpp(true);
    bcn_build.file("src/textures/bcn_shim.cpp");
    bcn_build.file(format!("{zuyu_stb_dir}/stb_dxt.cpp"));
    bcn_build.file(format!("{zuyu_bc_decoder_dir}/bc_decoder.cpp"));
    bcn_build.include(&zuyu_stb_dir);
    bcn_build.include(&zuyu_bc_decoder_dir);
    bcn_build.warnings(false);
    bcn_build.compile("ruzu_video_core_bcn_shim");

    compile_vulkan_present_shaders(&zuyu_base);
}

fn compile_vulkan_present_shaders(zuyu_base: &str) {
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::process::Command;

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").expect("OUT_DIR must be set"));
    let shader_dir = Path::new(zuyu_base).join("src/video_core/host_shaders");
    let local_shader_dir = Path::new("src/host_shaders");
    println!(
        "cargo:rerun-if-changed={}",
        local_shader_dir.join("ffx_a.h").display()
    );
    println!(
        "cargo:rerun-if-changed={}",
        local_shader_dir.join("ffx_fsr1.h").display()
    );
    let shaders = [
        ("VULKAN_PRESENT_VERT_SPV", "vulkan_present.vert"),
        ("VULKAN_PRESENT_FRAG_SPV", "vulkan_present.frag"),
        ("FXAA_VERT_SPV", "fxaa.vert"),
        ("FXAA_FRAG_SPV", "fxaa.frag"),
        ("SMAA_EDGE_DETECTION_VERT_SPV", "smaa_edge_detection.vert"),
        ("SMAA_EDGE_DETECTION_FRAG_SPV", "smaa_edge_detection.frag"),
        (
            "SMAA_BLENDING_WEIGHT_CALCULATION_VERT_SPV",
            "smaa_blending_weight_calculation.vert",
        ),
        (
            "SMAA_BLENDING_WEIGHT_CALCULATION_FRAG_SPV",
            "smaa_blending_weight_calculation.frag",
        ),
        (
            "SMAA_NEIGHBORHOOD_BLENDING_VERT_SPV",
            "smaa_neighborhood_blending.vert",
        ),
        (
            "SMAA_NEIGHBORHOOD_BLENDING_FRAG_SPV",
            "smaa_neighborhood_blending.frag",
        ),
        ("FULL_SCREEN_TRIANGLE_VERT_SPV", "full_screen_triangle.vert"),
        ("BLIT_COLOR_FLOAT_FRAG_SPV", "blit_color_float.frag"),
        (
            "VULKAN_BLIT_DEPTH_STENCIL_FRAG_SPV",
            "vulkan_blit_depth_stencil.frag",
        ),
        ("VULKAN_COLOR_CLEAR_VERT_SPV", "vulkan_color_clear.vert"),
        ("VULKAN_COLOR_CLEAR_FRAG_SPV", "vulkan_color_clear.frag"),
        (
            "VULKAN_DEPTHSTENCIL_CLEAR_FRAG_SPV",
            "vulkan_depthstencil_clear.frag",
        ),
        (
            "CONVERT_DEPTH_TO_FLOAT_FRAG_SPV",
            "convert_depth_to_float.frag",
        ),
        (
            "CONVERT_FLOAT_TO_DEPTH_FRAG_SPV",
            "convert_float_to_depth.frag",
        ),
        (
            "CONVERT_ABGR8_TO_D24S8_FRAG_SPV",
            "convert_abgr8_to_d24s8.frag",
        ),
        (
            "CONVERT_ABGR8_TO_D32F_FRAG_SPV",
            "convert_abgr8_to_d32f.frag",
        ),
        (
            "CONVERT_D32F_TO_ABGR8_FRAG_SPV",
            "convert_d32f_to_abgr8.frag",
        ),
        (
            "CONVERT_D24S8_TO_ABGR8_FRAG_SPV",
            "convert_d24s8_to_abgr8.frag",
        ),
        (
            "CONVERT_S8D24_TO_ABGR8_FRAG_SPV",
            "convert_s8d24_to_abgr8.frag",
        ),
        (
            "VULKAN_FIDELITYFX_FSR_VERT_SPV",
            "vulkan_fidelityfx_fsr.vert",
        ),
        (
            "VULKAN_FIDELITYFX_FSR_EASU_FP32_FRAG_SPV",
            "vulkan_fidelityfx_fsr_easu_fp32.frag",
        ),
        (
            "VULKAN_FIDELITYFX_FSR_EASU_FP16_FRAG_SPV",
            "vulkan_fidelityfx_fsr_easu_fp16.frag",
        ),
        (
            "VULKAN_FIDELITYFX_FSR_RCAS_FP32_FRAG_SPV",
            "vulkan_fidelityfx_fsr_rcas_fp32.frag",
        ),
        (
            "VULKAN_FIDELITYFX_FSR_RCAS_FP16_FRAG_SPV",
            "vulkan_fidelityfx_fsr_rcas_fp16.frag",
        ),
        ("PRESENT_BICUBIC_FRAG_SPV", "present_bicubic.frag"),
        ("PRESENT_GAUSSIAN_FRAG_SPV", "present_gaussian.frag"),
        (
            "VULKAN_PRESENT_SCALEFORCE_FP16_FRAG_SPV",
            "vulkan_present_scaleforce_fp16.frag",
        ),
        (
            "VULKAN_PRESENT_SCALEFORCE_FP32_FRAG_SPV",
            "vulkan_present_scaleforce_fp32.frag",
        ),
    ];

    let glslang = std::env::var("GLSLANGVALIDATOR").unwrap_or_else(|_| "glslangValidator".into());
    let mut generated = String::from("// Generated by video_core/build.rs.\n\n");

    for (name, filename) in shaders {
        let source = shader_dir.join(filename);
        println!("cargo:rerun-if-changed={}", source.display());
        let spv_path = out_dir.join(format!("{filename}.spv"));
        let output = Command::new(&glslang)
            .args(["-V", "--quiet", "--target-env", "spirv1.3", "-o"])
            .arg(&spv_path)
            .arg(format!("-I{}", shader_dir.display()))
            .arg(format!("-I{}", local_shader_dir.display()))
            .arg(&source)
            .output()
            .unwrap_or_else(|err| panic!("failed to run {glslang}: {err}"));
        if !output.status.success() {
            panic!(
                "failed to compile {} with {}:\nstdout:\n{}\nstderr:\n{}",
                source.display(),
                glslang,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }

        let bytes = fs::read(&spv_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", spv_path.display()));
        assert!(
            bytes.len() % 4 == 0,
            "SPIR-V output for {filename} is not word-aligned"
        );

        generated.push_str(&format!("pub const {name}: &[u32] = &[\n"));
        for chunk in bytes.chunks_exact(4) {
            let word = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            generated.push_str(&format!("    0x{word:08x},\n"));
        }
        generated.push_str("];\n\n");
    }

    fs::write(out_dir.join("vulkan_present_spv.rs"), generated)
        .expect("failed to write generated Vulkan present SPIR-V Rust module");
}

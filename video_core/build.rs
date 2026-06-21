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
}

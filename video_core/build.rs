fn main() {
    println!("cargo:rerun-if-changed=src/host1x/ffmpeg/ffmpeg_shim.c");
    println!("cargo:rerun-if-changed=src/textures/bcn_shim.cpp");

    const ZUYU_STB_DIR: &str = "/home/vricosti/Dev/emulators/zuyu/externals/stb";
    const ZUYU_BC_DECODER_DIR: &str = "/home/vricosti/Dev/emulators/zuyu/externals/bc_decoder";
    println!("cargo:rerun-if-changed={ZUYU_STB_DIR}/stb_dxt.cpp");
    println!("cargo:rerun-if-changed={ZUYU_STB_DIR}/stb_dxt.h");
    println!("cargo:rerun-if-changed={ZUYU_BC_DECODER_DIR}/bc_decoder.cpp");
    println!("cargo:rerun-if-changed={ZUYU_BC_DECODER_DIR}/bc_decoder.h");

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
    bcn_build.file(format!("{ZUYU_STB_DIR}/stb_dxt.cpp"));
    bcn_build.file(format!("{ZUYU_BC_DECODER_DIR}/bc_decoder.cpp"));
    bcn_build.include(ZUYU_STB_DIR);
    bcn_build.include(ZUYU_BC_DECODER_DIR);
    bcn_build.warnings(false);
    bcn_build.compile("ruzu_video_core_bcn_shim");
}

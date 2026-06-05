fn main() {
    println!("cargo:rerun-if-changed=src/host1x/ffmpeg/ffmpeg_shim.c");

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
}

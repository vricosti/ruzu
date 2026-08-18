fn main() {
    // `adsp::apps::opus` binds libopus through `#[link(name = "opus")]`, which
    // names the library but never tells the linker where to find it. Probing
    // with pkg-config emits the matching `cargo:rustc-link-search`, which is
    // required wherever libopus lives outside the linker's default paths
    // (Homebrew's `/opt/homebrew/lib` on Apple Silicon, for instance).
    pkg_config::Config::new()
        .probe("opus")
        .expect("libopus is required for audio_core Opus decoding");
}

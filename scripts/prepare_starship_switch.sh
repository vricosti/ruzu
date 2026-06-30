#!/usr/bin/env bash
set -euo pipefail

ROM_PATH="${1:-/Users/vricosti/Games/Emulators/Switch/roms/Star Fox 64 (USA) (Rev 1).z64}"
BASE_DIR="${STARSHIP_BASE_DIR:-/Users/vricosti/Dev/emulators}"
STARSHIP_DIR="${STARSHIP_DIR:-$BASE_DIR/starship}"
OUT_DIR="${2:-$BASE_DIR/starship-switch-test}"
RUZU_DIR="${RUZU_DIR:-/Users/vricosti/Dev/emulators/ruzu}"
DEVKITPRO="${DEVKITPRO:-/opt/devkitpro}"
SWITCH_CMAKE="${SWITCH_CMAKE:-$DEVKITPRO/cmake/Switch.cmake}"
LIBULTRASHIP_NX_REV="${LIBULTRASHIP_NX_REV:-09dfab5fb2a9a047a6e268dc9db2daad9b2ce5f0}"

US_10_SHA1="d8b1088520f7c5f81433292a9258c1184afa1457"
US_11_SHA1="09f0d105f476b00efa5303a3ebc42e60a7753b7a"

die() {
    printf 'error: %s\n' "$*" >&2
    exit 1
}

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "missing command '$1'"
}

cpu_count() {
    if command -v sysctl >/dev/null 2>&1; then
        sysctl -n hw.ncpu
    else
        getconf _NPROCESSORS_ONLN
    fi
}

printf 'ROM: %s\n' "$ROM_PATH"
printf 'Starship repo: %s\n' "$STARSHIP_DIR"
printf 'Output dir: %s\n' "$OUT_DIR"

require_cmd git
require_cmd cmake
require_cmd ninja
require_cmd shasum
require_cmd ruby
require_cmd dkp-pacman

[[ -f "$ROM_PATH" ]] || die "ROM not found: $ROM_PATH"
[[ -f "$SWITCH_CMAKE" ]] || die "Switch toolchain not found: $SWITCH_CMAKE; install devkitPro/devkitA64 first"
dkp-pacman -Q switch-zlib >/dev/null 2>&1 || die "missing devkitPro package switch-zlib; run: sudo dkp-pacman -S --needed switch-zlib switch-bzip2"
missing_switch_packages=()
for package in switch-sdl2 switch-libogg switch-libvorbis switch-glfw switch-mesa; do
    if ! dkp-pacman -Q "$package" >/dev/null 2>&1; then
        missing_switch_packages+=("$package")
    fi
done
if ((${#missing_switch_packages[@]})); then
    die "missing devkitPro packages: ${missing_switch_packages[*]}; run: sudo dkp-pacman -S --needed switch-zlib switch-bzip2 switch-sdl2 switch-libogg switch-libvorbis switch-glfw switch-mesa"
fi

rom_sha1="$(shasum -a 1 "$ROM_PATH" | awk '{print tolower($1)}')"
case "$rom_sha1" in
    "$US_10_SHA1")
        printf 'ROM hash OK: Star Fox 64 US 1.0\n'
        ;;
    "$US_11_SHA1")
        printf 'ROM hash OK: Star Fox 64 US 1.1 Rev A\n'
        ;;
    *)
        die "unsupported ROM SHA1: $rom_sha1"
        ;;
esac

mkdir -p "$BASE_DIR"
if [[ ! -d "$STARSHIP_DIR/.git" ]]; then
    git clone --recursive https://github.com/HarbourMasters/Starship.git "$STARSHIP_DIR"
else
    git -C "$STARSHIP_DIR" fetch origin
    git -C "$STARSHIP_DIR" submodule update --init --recursive
fi

cd "$STARSHIP_DIR"
cp "$ROM_PATH" baserom.z64

printf '\n== Generating Starship assets ==\n'
if [[ "${FORCE_STARSHIP_ASSETS:-0}" != "1" && -f build-cmake/sf64.o2r && -f build-cmake/starship.o2r ]]; then
    printf 'Using existing build-cmake/sf64.o2r and build-cmake/starship.o2r\n'
else
    cmake -S tools/Torch -B tools/Torch/cmake-build-release -GNinja -DCMAKE_BUILD_TYPE=Release
    ruby -0pi -e 'gsub(/#\s*define SPDLOG_FMT_STRING\(format_string\) FMT_STRING\(format_string\)/, "#    define SPDLOG_FMT_STRING(format_string) format_string")' \
        tools/Torch/cmake-build-release/_deps/spdlog-src/include/spdlog/common.h
    cmake --build tools/Torch/cmake-build-release -j"$(cpu_count)"
    tools/Torch/cmake-build-release/torch o2r baserom.z64
    tools/Torch/cmake-build-release/torch pack port starship.o2r o2r
    mkdir -p build-cmake
    cp sf64.o2r build-cmake/sf64.o2r
    cp starship.o2r build-cmake/starship.o2r
fi

[[ -f build-cmake/sf64.o2r ]] || die "missing build-cmake/sf64.o2r"
[[ -f build-cmake/starship.o2r ]] || die "missing build-cmake/starship.o2r"

printf '\n== Preparing libultraship Switch backend ==\n'
if ! git -C libultraship remote get-url nx >/dev/null 2>&1; then
    git -C libultraship remote add nx https://github.com/Net64DD/libultraship.git
fi
git -C libultraship fetch nx
git -C libultraship checkout "$LIBULTRASHIP_NX_REV"

if ! grep -q "codex switch dependency fallback" libultraship/src/CMakeLists.txt; then
    ruby -0pi -e 'sub(%r{find_package\(nlohmann_json REQUIRED\)}, %q{if (CMAKE_SYSTEM_NAME STREQUAL "NintendoSwitch")
    # codex switch dependency fallback: devkitPro macOS does not currently ship all required CMake packages.
    include(FetchContent)
    find_package(nlohmann_json QUIET)
    if (NOT ${nlohmann_json_FOUND})
        FetchContent_Declare(
            nlohmann_json
            GIT_REPOSITORY https://github.com/nlohmann/json.git
            GIT_TAG v3.11.3
            OVERRIDE_FIND_PACKAGE
        )
        FetchContent_MakeAvailable(nlohmann_json)
    endif()

    find_package(tinyxml2 QUIET)
    if (NOT ${tinyxml2_FOUND})
        set(tinyxml2_BUILD_TESTING OFF)
        FetchContent_Declare(
            tinyxml2
            GIT_REPOSITORY https://github.com/leethomason/tinyxml2.git
            GIT_TAG 10.0.0
            OVERRIDE_FIND_PACKAGE
        )
        FetchContent_MakeAvailable(tinyxml2)
    endif()

    find_package(spdlog QUIET)
    if (NOT ${spdlog_FOUND})
        FetchContent_Declare(
            spdlog
            GIT_REPOSITORY https://github.com/gabime/spdlog.git
            GIT_TAG v1.14.1
            OVERRIDE_FIND_PACKAGE
        )
        FetchContent_MakeAvailable(spdlog)
    endif()

    find_package(libzip QUIET)
    if (NOT ${libzip_FOUND})
        set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
        set(BUILD_TOOLS OFF)
        set(BUILD_REGRESS OFF)
        set(BUILD_EXAMPLES OFF)
        set(BUILD_DOC OFF)
        set(BUILD_OSSFUZZ OFF)
        set(BUILD_SHARED_LIBS OFF)
        FetchContent_Declare(
            libzip
            GIT_REPOSITORY https://github.com/nih-at/libzip.git
            GIT_TAG v1.10.1
            OVERRIDE_FIND_PACKAGE
        )
        FetchContent_MakeAvailable(libzip)
        list(APPEND ADDITIONAL_LIB_INCLUDES ${libzip_SOURCE_DIR}/lib ${libzip_BINARY_DIR})
    endif()
endif()

find_package(nlohmann_json REQUIRED)})' libultraship/src/CMakeLists.txt
fi

if ! grep -q "codex switch libzip fallback" libultraship/src/CMakeLists.txt; then
    ruby -0pi -e 'sub(%r{find_package\(libzip REQUIRED\)}, %q{if (CMAKE_SYSTEM_NAME STREQUAL "NintendoSwitch" AND FALSE)
    # codex switch libzip fallback: handled before dependency find_package calls.
    include(FetchContent)
    set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
    set(BUILD_TOOLS OFF)
    set(BUILD_REGRESS OFF)
    set(BUILD_EXAMPLES OFF)
    set(BUILD_DOC OFF)
    set(BUILD_OSSFUZZ OFF)
    set(BUILD_SHARED_LIBS OFF)
    FetchContent_Declare(
        libzip
        GIT_REPOSITORY https://github.com/nih-at/libzip.git
        GIT_TAG v1.10.1
        OVERRIDE_FIND_PACKAGE
    )
    FetchContent_MakeAvailable(libzip)
    list(APPEND ADDITIONAL_LIB_INCLUDES ${libzip_SOURCE_DIR}/lib ${libzip_BINARY_DIR})
endif()

find_package(libzip REQUIRED)})' libultraship/src/CMakeLists.txt
fi

if grep -q "codex switch global include fallback" CMakeLists.txt; then
    ruby -0pi -e 'gsub(/\nif \(CMAKE_SYSTEM_NAME STREQUAL "NintendoSwitch"\)\n    # codex switch global include fallback:.*?\nendif\(\)\n/m, "\n")' CMakeLists.txt
fi

if ! grep -q "codex switch imgui include fallback" libultraship/cmake/dependencies/common.cmake; then
    ruby -0pi -e 'sub(/(add_library\(ImGui STATIC\)\n)/) { "#{$1}\nif (CMAKE_SYSTEM_NAME STREQUAL \"NintendoSwitch\")\n    # codex switch imgui include fallback: keep portlibs includes scoped away from C libzip config.h.\n    target_include_directories(ImGui PUBLIC $ENV{DEVKITPRO}/portlibs/switch/include $ENV{DEVKITPRO}/portlibs/switch/include/SDL2)\n    target_compile_definitions(ImGui PUBLIC IMGUI_IMPL_OPENGL_ES3 IMGUI_DISABLE_DEFAULT_SHELL_FUNCTIONS)\nendif()\n" }' libultraship/cmake/dependencies/common.cmake
else
    ruby -0pi -e 'gsub("target_compile_definitions(ImGui PUBLIC IMGUI_IMPL_OPENGL_ES3)", "target_compile_definitions(ImGui PUBLIC IMGUI_IMPL_OPENGL_ES3 IMGUI_DISABLE_DEFAULT_SHELL_FUNCTIONS)")' \
        libultraship/cmake/dependencies/common.cmake
fi

ruby -0pi -e 'gsub("target_link_libraries(libultraship PRIVATE libzip::zip)", "target_link_libraries(libultraship PUBLIC libzip::zip)")' \
    libultraship/src/CMakeLists.txt
ruby -0pi -e 'gsub(/^\s*-lglad\n/, "")' CMakeLists.txt

mkdir -p src/port/switch
cat > src/port/switch/SwitchImpl.h <<'EOF'
#pragma once

namespace Ship {

enum InitPhase {
    PreInitPhase,
    PostInitPhase,
};

namespace Switch {

inline void Init(InitPhase) {}
inline void Exit() {}
inline void ApplyOverclock() {}

} // namespace Switch
} // namespace Ship
EOF

cat > src/port/switch/SwitchPerformanceProfiles.h <<'EOF'
#pragma once

namespace Ship {

enum class SwitchProfiles {
    STOCK = 0,
    BOOST,
    MAX,
};

} // namespace Ship

static const char* SWITCH_CPU_PROFILES[] = {
    "Stock",
    "Boost",
    "Max",
};
EOF

if ! grep -q "!defined(USE_OPENGLES)" libultraship/src/graphic/Fast3D/backends/gfx_opengl.cpp; then
    ruby -0pi -e 'sub("#ifndef __linux__\n    glewInit();\n#endif", "#if !defined(__linux__) && !defined(USE_OPENGLES)\n    glewInit();\n#endif")' \
        libultraship/src/graphic/Fast3D/backends/gfx_opengl.cpp
fi

printf '\n== Building Starship.nro ==\n'
cmake -H. -Bbuild-switch -GNinja -DCMAKE_TOOLCHAIN_FILE="$SWITCH_CMAKE" \
    -DUSE_OPENGLES=ON

if [[ -f build-switch/_deps/libzip-src/lib/CMakeLists.txt ]] && \
    ! grep -q "target_include_directories(zip BEFORE" build-switch/_deps/libzip-src/lib/CMakeLists.txt; then
    ruby -0pi -e 'sub("target_include_directories(zip\n", "target_include_directories(zip BEFORE\n")' \
        build-switch/_deps/libzip-src/lib/CMakeLists.txt
    cmake -H. -Bbuild-switch -GNinja -DCMAKE_TOOLCHAIN_FILE="$SWITCH_CMAKE" \
        -DUSE_OPENGLES=ON
fi

if [[ -f build-switch/_deps/spdlog-src/include/spdlog/details/os-inl.h ]]; then
    ruby -0pi -e 'gsub("int fd = ::fileno(f);", "int fd = __sfileno(f);"); gsub(/return ::_isatty\(.*?file\)\) != 0;/, "return ::_isatty(__sfileno(file)) != 0;"); gsub(/return ::isatty\(.*?file\)\) != 0;/, "return ::isatty(__sfileno(file)) != 0;"); gsub("return ::fsync(__sfileno(fp)) == 0;", "return true;"); gsub("return ::fsync(fileno(fp)) == 0;", "return true;")' \
        build-switch/_deps/spdlog-src/include/spdlog/details/os-inl.h
fi
if [[ -f build-switch/_deps/imgui-src/backends/imgui_impl_sdl2.cpp ]]; then
    ruby -0pi -e 'gsub("#elif SDL_HAS_OPEN_URL", "#elif SDL_HAS_OPEN_URL && !defined(__SWITCH__)")' \
        build-switch/_deps/imgui-src/backends/imgui_impl_sdl2.cpp
fi

cmake --build build-switch --config Release -j"$(cpu_count)"

nro_path="$(find build-switch -maxdepth 1 -name '*.nro' -print -quit)"
[[ -n "$nro_path" ]] || die "no .nro found in build-switch"

printf '\n== Staging ruzu test directory ==\n'
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
cp "$nro_path" "$OUT_DIR/Starship.nro"
cp build-cmake/sf64.o2r "$OUT_DIR/"
cp build-cmake/starship.o2r "$OUT_DIR/"
cp config.yml "$OUT_DIR/"
cp -R assets "$OUT_DIR/"

if [[ ! -f "$OUT_DIR/gamecontrollerdb.txt" ]]; then
    curl -fsSL https://raw.githubusercontent.com/mdqinc/SDL_GameControllerDB/master/gamecontrollerdb.txt \
        -o "$OUT_DIR/gamecontrollerdb.txt" || true
fi

printf '\nReady.\n'
printf 'Run with:\n'
printf '  cd %q\n' "$OUT_DIR"
printf '  RUST_LOG=info timeout 30s %q/target/release/ruzu-cmd -g ./Starship.nro\n' "$RUZU_DIR"

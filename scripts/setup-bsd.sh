#!/bin/sh
# Install the tools and native libraries required to build ruzu on BSD.
set -eu

PLATFORM_SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
PROJECT_DIR=$(CDPATH= cd -- "${PLATFORM_SCRIPT_DIR}/.." && pwd)
# shellcheck source=setup-common.sh
. "${PLATFORM_SCRIPT_DIR}/setup-common.sh"

case "$(uname -s)" in
    FreeBSD)
        PLATFORM_NAME="FreeBSD $(freebsd-version -u 2>/dev/null || uname -r)"
        PACKAGE_MANAGER=freebsd-pkg
        REQUIRED_PACKAGES="
            alsa-lib cmake curl ffmpeg git glslang gtk4 jackit libX11
            ninja pkgconf pulseaudio vulkan-headers vulkan-loader
            vulkan-tools
        "
        ;;
    NetBSD)
        PLATFORM_NAME="NetBSD $(uname -r)"
        PACKAGE_MANAGER=pkgin
        REQUIRED_PACKAGES="
            alsa-lib cmake curl ffmpeg7 git glslang gtk4 jack
            ninja-build openssl pkgconf pulseaudio vulkan-headers
            vulkan-loader
        "
        ;;
    OpenBSD)
        PLATFORM_NAME="OpenBSD $(uname -r)"
        PACKAGE_MANAGER=openbsd-pkg
        REQUIRED_PACKAGES="
            cmake curl ffmpeg git glslang gmake gtk+4 ninja
            vulkan-headers vulkan-loader
        "
        ;;
    *)
        echo "Unsupported BSD operating system: $(uname -s)." >&2
        exit 1
        ;;
esac

package_installed() {
    package_name=$1
    case "$PACKAGE_MANAGER" in
        freebsd-pkg)
            pkg info -e "$package_name" >/dev/null 2>&1
            ;;
        pkgin|openbsd-pkg)
            pkg_info -e "$package_name-*" >/dev/null 2>&1
            ;;
    esac
}

install_packages() {
    # Word splitting is intentional: package names cannot contain whitespace.
    # shellcheck disable=SC2086
    case "$PACKAGE_MANAGER" in
        freebsd-pkg)
            run_privileged env ASSUME_ALWAYS_YES=yes pkg install $MISSING_PACKAGES
            ;;
        pkgin)
            run_privileged pkgin -y update
            run_privileged pkgin -y install $MISSING_PACKAGES
            ;;
        openbsd-pkg)
            run_privileged pkg_add $MISSING_PACKAGES
            ;;
    esac
}

configure_native_paths() {
    case "$PACKAGE_MANAGER" in
        pkgin)
            # pkgsrc keeps versioned FFmpeg metadata outside pkg-config's
            # default search path.
            if [ -d /usr/pkg/lib/ffmpeg7/pkgconfig ]; then
                # The glob is intentional and is guaranteed by ffmpeg7.
                # shellcheck disable=SC2086
                run_privileged ln -sf \
                    /usr/pkg/lib/ffmpeg7/pkgconfig/*.pc \
                    /usr/pkg/lib/pkgconfig/
            fi
            ;;
        openbsd-pkg)
            # cubeb-sys currently emits -lstdc++ on OpenBSD even though the
            # system compiler and libcubeb use libc++. Provide a project-local
            # compatibility name; .cargo/config.toml adds this directory to
            # the native linker search path only on OpenBSD.
            cxx_library=$(c++ -print-file-name=libc++.so 2>/dev/null || true)
            if [ -z "$cxx_library" ] || [ ! -f "$cxx_library" ]; then
                echo "Unable to locate OpenBSD's libc++ shared library." >&2
                return 1
            fi
            mkdir -p "$PROJECT_DIR/.cargo/openbsd-lib"
            ln -sf "$cxx_library" "$PROJECT_DIR/.cargo/openbsd-lib/libstdc++.so"
            ;;
    esac
}

print_platform_build_notes() {
    if [ "$(uname -s)" = OpenBSD ]; then
        cat <<'EOF'

OpenBSD's default per-process data limit is too small for this project.
Raise it in the shell that will run Cargo:

  ulimit -d 6291456
  cargo build --locked --bin ruzu -j 1
EOF
    fi
}

run_setup

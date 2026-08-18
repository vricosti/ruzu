#!/bin/sh
# Install the tools and native libraries required to build ruzu on Linux.
set -eu

PLATFORM_SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=setup-common.sh
. "${PLATFORM_SCRIPT_DIR}/setup-common.sh"

if [ ! -r /etc/os-release ]; then
    echo "Unable to determine the Linux distribution." >&2
    exit 1
fi

# shellcheck disable=SC1091
. /etc/os-release
PLATFORM_NAME=${PRETTY_NAME:-${ID:-Linux}}

case "${ID:-}" in
    ubuntu|debian)
        PACKAGE_MANAGER=apt
        REQUIRED_PACKAGES="
            build-essential ca-certificates clang cmake curl git
            glslang-tools libasound2-dev libavcodec-dev libavutil-dev
            libclang-dev libgtk-4-dev libjack-jackd2-dev libpulse-dev
            libssl-dev libvulkan-dev libx11-dev
            ninja-build pkg-config vulkan-tools
        "
        ;;
    fedora)
        PACKAGE_MANAGER=dnf
        REQUIRED_PACKAGES="
            alsa-lib-devel ca-certificates clang clang-devel cmake
            curl ffmpeg-free-devel gcc gcc-c++ git
            glslang gtk4-devel jack-audio-connection-kit-devel
            libX11-devel make ninja-build openssl-devel pkgconf-pkg-config
            pulseaudio-libs-devel vulkan-headers
            vulkan-loader-devel vulkan-tools
        "
        ;;
    arch|manjaro|endeavouros)
        PACKAGE_MANAGER=pacman
        REQUIRED_PACKAGES="
            base-devel alsa-lib ca-certificates clang cmake curl
            ffmpeg git glslang gtk4 jack2 libpulse libx11 ninja
            openssl pkgconf vulkan-headers vulkan-icd-loader
            vulkan-tools
        "
        ;;
    opensuse-tumbleweed|opensuse-leap)
        PACKAGE_MANAGER=zypper
        REQUIRED_PACKAGES="
            alsa-devel ca-certificates clang clang-devel cmake curl
            ffmpeg-8-libavcodec-devel ffmpeg-8-libavutil-devel
            gcc gcc-c++ git glslang-devel gtk4-devel
            libX11-devel libjack-devel libopenssl-devel
            libpulse-devel make ninja pkgconf-pkg-config
            vulkan-devel vulkan-tools
        "
        ;;
    alpine)
        PACKAGE_MANAGER=apk
        REQUIRED_PACKAGES="
            alsa-lib-dev build-base ca-certificates clang clang-dev
            cmake curl ffmpeg-dev git glslang-dev gtk4.0-dev jack-dev
            libx11-dev ninja openssl-dev pkgconf pulseaudio-dev
            vulkan-headers vulkan-loader-dev vulkan-tools
        "
        ;;
    *)
        echo "Unsupported Linux distribution: $PLATFORM_NAME." >&2
        exit 1
        ;;
esac

package_installed() {
    package_name=$1
    case "$PACKAGE_MANAGER" in
        apt)
            [ "$(dpkg-query -W -f='${db:Status-Abbrev}' "$package_name" 2>/dev/null || true)" = "ii " ]
            ;;
        dnf|zypper)
            rpm -q "$package_name" >/dev/null 2>&1
            ;;
        pacman)
            pacman -Q "$package_name" >/dev/null 2>&1
            ;;
        apk)
            apk info -e "$package_name" >/dev/null 2>&1
            ;;
    esac
}

install_packages() {
    # Word splitting is intentional: package names cannot contain whitespace.
    # shellcheck disable=SC2086
    case "$PACKAGE_MANAGER" in
        apt)
            run_privileged env DEBIAN_FRONTEND=noninteractive apt-get update
            run_privileged env DEBIAN_FRONTEND=noninteractive \
                apt-get install -y --no-install-recommends $MISSING_PACKAGES
            ;;
        dnf)
            run_privileged dnf install -y $MISSING_PACKAGES
            ;;
        pacman)
            run_privileged pacman -Syu --needed --noconfirm $MISSING_PACKAGES
            ;;
        zypper)
            run_privileged zypper --non-interactive refresh
            run_privileged zypper --non-interactive install --no-recommends $MISSING_PACKAGES
            ;;
        apk)
            run_privileged apk add --no-cache $MISSING_PACKAGES
            ;;
    esac
}

run_setup

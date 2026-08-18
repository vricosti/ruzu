#!/bin/sh
# Install the tools and native libraries required to build ruzu on macOS.
set -eu

PLATFORM_SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=setup-common.sh
. "${PLATFORM_SCRIPT_DIR}/setup-common.sh"

if [ "$(uname -s)" != Darwin ]; then
    echo "This setup script only supports macOS." >&2
    exit 1
fi

PLATFORM_NAME="$(sw_vers -productName) $(sw_vers -productVersion)"
PACKAGE_MANAGER=brew
REQUIRED_PACKAGES="
    cmake ffmpeg glslang gtk4 molten-vk ninja openssl@3
    pkgconf vulkan-headers vulkan-loader vulkan-tools
"

load_homebrew() {
    if command -v brew >/dev/null 2>&1; then
        return 0
    fi
    if [ -x /opt/homebrew/bin/brew ]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [ -x /usr/local/bin/brew ]; then
        eval "$(/usr/local/bin/brew shellenv)"
    fi
}

ensure_homebrew() {
    load_homebrew
    if command -v brew >/dev/null 2>&1; then
        return 0
    fi

    echo "[MISSING] Homebrew is not installed."
    if ! confirm_install "Install Homebrew?"; then
        echo "Homebrew installation declined."
        return 1
    fi

    NONINTERACTIVE=1 /bin/bash -c \
        "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    load_homebrew
    command -v brew >/dev/null 2>&1
}

prepare_platform() {
    if ! ensure_homebrew; then
        echo "Setup is incomplete because Homebrew is required on macOS." >&2
        exit 1
    fi
    if ! xcrun --find clang >/dev/null 2>&1; then
        cat >&2 <<'EOF'
Apple Command Line Tools are missing. Run `xcode-select --install`, finish the
installer, then rerun this script. Homebrew and the native compiler require
these tools.
EOF
        exit 1
    fi
}

package_installed() {
    brew list --versions "$1" >/dev/null 2>&1
}

install_packages() {
    # Word splitting is intentional: package names cannot contain whitespace.
    # shellcheck disable=SC2086
    brew install $MISSING_PACKAGES
}

run_setup

#!/usr/bin/env bash
set -euo pipefail

PKG_URL="https://github.com/devkitPro/pacman/releases/latest/download/devkitpro-pacman-installer.pkg"
PKG_PATH="/tmp/devkitpro-pacman-installer.pkg"

if [[ "$(uname -s)" != "Darwin" ]]; then
    echo "This installer is for macOS only." >&2
    exit 1
fi

if [[ "${EUID}" -ne 0 ]]; then
    echo "Run with sudo:" >&2
    echo "  sudo $0" >&2
    exit 1
fi

echo "Downloading devkitPro pacman installer..."
curl -L --fail --show-error --output "${PKG_PATH}" "${PKG_URL}"

echo "Installing devkitPro pacman..."
installer -pkg "${PKG_PATH}" -target /

if ! command -v dkp-pacman >/dev/null 2>&1; then
    if [[ -x /opt/devkitpro/pacman/bin/dkp-pacman ]]; then
        export PATH="/opt/devkitpro/pacman/bin:${PATH}"
    else
        echo "dkp-pacman was not found after package installation." >&2
        exit 1
    fi
fi

echo "Updating devkitPro package database..."
dkp-pacman -Syu --noconfirm

echo "Installing Switch development toolchain..."
dkp-pacman -S --needed --noconfirm switch-dev

cat <<'EOF'

devkitPro Switch toolchain installed.

Add this to your shell profile if it is not already present:

  export DEVKITPRO=/opt/devkitpro
  export DEVKITARM=/opt/devkitpro/devkitARM
  export DEVKITPPC=/opt/devkitpro/devkitPPC
  export PATH=/opt/devkitpro/tools/bin:$PATH

Then reload your shell:

  source ~/.zshrc

EOF

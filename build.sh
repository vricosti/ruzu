#!/bin/sh
# Build ruzu: check the platform dependencies, then compile the workspace.
#
# Dispatches to the per-OS script, which installs anything missing and then
# runs the build. Release is the default; pass --debug for a debug build.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

usage() {
    cat <<'EOF'
Usage: ./build.sh [options] [-- <extra cargo arguments>]

Options:
  --debug        Build the debug profile instead of release.
  --deps-only    Only check and install dependencies; do not build.
  --skip-deps    Skip the dependency check and build straight away.
  -h, --help     Show this help.

Everything after `--` is forwarded to `cargo build`, so a single crate can be
built with, for example:

  ./build.sh -- --bin ruzu-cmd
EOF
}

case "${1-}" in
    -h|--help)
        usage
        exit 0
        ;;
esac

case "$(uname -s)" in
    Linux)
        PLATFORM_BUILD="${SCRIPT_DIR}/scripts/build-linux.sh"
        ;;
    FreeBSD|NetBSD|OpenBSD)
        PLATFORM_BUILD="${SCRIPT_DIR}/scripts/build-bsd.sh"
        ;;
    Darwin)
        PLATFORM_BUILD="${SCRIPT_DIR}/scripts/build-macos.sh"
        ;;
    *)
        echo "Unsupported operating system: $(uname -s)." >&2
        exit 1
        ;;
esac

if [ ! -x "$PLATFORM_BUILD" ]; then
    echo "Platform build script is missing or not executable: $PLATFORM_BUILD" >&2
    exit 1
fi

exec "$PLATFORM_BUILD" "$@"

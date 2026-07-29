#!/bin/sh
# Dispatch the dependency setup to the current operating-system family.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

case "$(uname -s)" in
    Linux)
        PLATFORM_SETUP="${SCRIPT_DIR}/scripts/setup-linux.sh"
        ;;
    FreeBSD|NetBSD|OpenBSD)
        PLATFORM_SETUP="${SCRIPT_DIR}/scripts/setup-bsd.sh"
        ;;
    Darwin)
        PLATFORM_SETUP="${SCRIPT_DIR}/scripts/setup-macos.sh"
        ;;
    *)
        echo "Unsupported operating system: $(uname -s)." >&2
        exit 1
        ;;
esac

if [ ! -x "$PLATFORM_SETUP" ]; then
    echo "Platform setup script is missing or not executable: $PLATFORM_SETUP" >&2
    exit 1
fi

exec "$PLATFORM_SETUP" "$@"

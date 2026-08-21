#!/bin/sh
# Shared dependency checks and build driver used by the per-OS build scripts.

RUST_MINIMUM=1.85.0
SETUP_COMPLETE=true

# Build options, overridden by parse_build_args.
BUILD_PROFILE=release
RUN_DEPENDENCY_CHECK=true
RUN_BUILD=true
CARGO_EXTRA_ARGS=

confirm_install() {
    prompt=$1
    printf '%s [y/N] ' "$prompt"
    IFS= read -r answer || answer=
    case "$answer" in
        y|Y|yes|Yes|YES) return 0 ;;
        *) return 1 ;;
    esac
}

run_privileged() {
    if [ "$(id -u)" -eq 0 ]; then
        "$@"
    elif command -v sudo >/dev/null 2>&1; then
        sudo "$@"
    elif command -v doas >/dev/null 2>&1; then
        doas "$@"
    else
        echo "Root privileges, sudo, or doas are required to install packages." >&2
        return 1
    fi
}

version_at_least() {
    current_version=$1
    minimum_version=$2
    current_major=${current_version%%.*}
    current_version=${current_version#*.}
    current_minor=${current_version%%.*}
    current_patch=${current_version#*.}
    minimum_major=${minimum_version%%.*}
    minimum_version=${minimum_version#*.}
    minimum_minor=${minimum_version%%.*}
    minimum_patch=${minimum_version#*.}

    [ "$current_major" -gt "$minimum_major" ] ||
        { [ "$current_major" -eq "$minimum_major" ] &&
          { [ "$current_minor" -gt "$minimum_minor" ] ||
            { [ "$current_minor" -eq "$minimum_minor" ] &&
              [ "$current_patch" -ge "$minimum_patch" ]; }; }; }
}

install_rustup() {
    if [ "$(uname -s)" = OpenBSD ]; then
        cat >&2 <<'EOF'
Rust installation cannot continue: the Rust project does not publish a
rustup host toolchain for OpenBSD. Install OpenBSD's rust package manually,
then rerun this script. The script will never silently replace rustup with a
distribution Rust package.
EOF
        return 1
    fi

    if ! command -v curl >/dev/null 2>&1; then
        echo "curl is required to install Rust with rustup." >&2
        return 1
    fi

    curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs |
        sh -s -- -y --profile minimal --default-toolchain stable
}

verify_native_libraries() {
    if ! command -v pkg-config >/dev/null 2>&1; then
        echo "[ERROR] pkg-config is unavailable after package installation." >&2
        return 1
    fi

    if ! pkg-config --atleast-version=4.6 gtk4; then
        gtk_version=$(pkg-config --modversion gtk4 2>/dev/null || echo "not found")
        echo "[ERROR] GTK 4.6 or newer is required (found: $gtk_version)." >&2
        return 1
    fi

    echo "[OK] GTK $(pkg-config --modversion gtk4) is available; Cargo builds SDL3 from source."
}

run_setup() {
    echo "Detected platform: $PLATFORM_NAME"

    MISSING_PACKAGES=
    echo "Checking system libraries and build tools..."
    # Word splitting is intentional: package names cannot contain whitespace.
    # shellcheck disable=SC2086
    for package_name in $REQUIRED_PACKAGES; do
        if package_installed "$package_name"; then
            printf '  [OK]      %s\n' "$package_name"
        else
            printf '  [MISSING] %s\n' "$package_name"
            MISSING_PACKAGES="${MISSING_PACKAGES}${MISSING_PACKAGES:+ }${package_name}"
        fi
    done

    if [ -n "$MISSING_PACKAGES" ]; then
        # Word splitting is intentional: package names cannot contain whitespace.
        # shellcheck disable=SC2086
        set -- $MISSING_PACKAGES
        package_count=$#
        echo
        if confirm_install "Install the $package_count missing system packages?"; then
            if ! install_packages; then
                echo "System package installation failed." >&2
                SETUP_COMPLETE=false
            fi
        else
            echo "System package installation declined."
            SETUP_COMPLETE=false
        fi
    else
        echo "All system dependencies are already installed."
    fi

    if command -v configure_native_paths >/dev/null 2>&1; then
        if ! configure_native_paths; then
            echo "Native library path configuration failed." >&2
            SETUP_COMPLETE=false
        fi
    fi

    if ! verify_native_libraries; then
        SETUP_COMPLETE=false
    fi

    export PATH="${HOME}/.cargo/bin:${PATH}"
    RUST_VERSION=
    if command -v rustc >/dev/null 2>&1 && command -v cargo >/dev/null 2>&1; then
        RUST_VERSION=$(rustc --version)
        RUST_VERSION=${RUST_VERSION#rustc }
        RUST_VERSION=${RUST_VERSION%% *}
    fi

    if [ -n "$RUST_VERSION" ] && version_at_least "$RUST_VERSION" "$RUST_MINIMUM"; then
        echo "[OK] Rust $RUST_VERSION and Cargo are installed."
    else
        if [ -n "$RUST_VERSION" ]; then
            echo "[MISSING] Rust $RUST_VERSION is too old (minimum: $RUST_MINIMUM)."
        else
            echo "[MISSING] Rust and/or Cargo are not installed."
        fi

        if confirm_install "Install the stable Rust toolchain with rustup?"; then
            if ! install_rustup; then
                SETUP_COMPLETE=false
            fi
            export PATH="${HOME}/.cargo/bin:${PATH}"
        else
            echo "Rust installation declined."
            SETUP_COMPLETE=false
        fi
    fi

    if [ "$SETUP_COMPLETE" != true ] ||
       ! command -v rustc >/dev/null 2>&1 ||
       ! command -v cargo >/dev/null 2>&1; then
        echo
        echo "Setup is incomplete: one or more installations were declined or failed." >&2
        exit 1
    fi

    echo
    echo "Dependency check completed on $PLATFORM_NAME."
    echo "Rust  : $(rustc --version)"
    echo "Cargo : $(cargo --version)"
    echo "All required dependencies are available."

    if command -v print_platform_build_notes >/dev/null 2>&1; then
        print_platform_build_notes
    fi
}

# Reads the options understood by build.sh. Everything after `--` is kept for
# cargo, so callers can target a single crate or add their own flags.
parse_build_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --debug)
                BUILD_PROFILE=debug
                ;;
            --release)
                BUILD_PROFILE=release
                ;;
            --deps-only)
                RUN_BUILD=false
                ;;
            --skip-deps)
                RUN_DEPENDENCY_CHECK=false
                ;;
            --)
                shift
                CARGO_EXTRA_ARGS="$*"
                break
                ;;
            *)
                echo "Unknown option: $1" >&2
                echo "Run ./build.sh --help for the available options." >&2
                exit 1
                ;;
        esac
        shift
    done
}

# Compiles the workspace. Runs after run_setup so it inherits any PATH or
# PKG_CONFIG_PATH the platform hook exported: on macOS in particular, Homebrew's
# pkg-config has to come first or every probe fails.
run_build() {
    REPO_ROOT=$(CDPATH= cd -- "${PLATFORM_SCRIPT_DIR}/.." && pwd)

    set -- build
    if [ "$BUILD_PROFILE" = release ]; then
        set -- "$@" --release
    fi
    if [ -n "$CARGO_EXTRA_ARGS" ]; then
        # Word splitting is intentional: these are separate cargo arguments.
        # shellcheck disable=SC2086
        set -- "$@" $CARGO_EXTRA_ARGS
    fi

    echo
    echo "Building the $BUILD_PROFILE profile..."
    echo "  cargo $*"
    if ! (cd "$REPO_ROOT" && cargo "$@"); then
        echo "Build failed." >&2
        exit 1
    fi

    echo
    echo "Build finished ($BUILD_PROFILE)."
    echo "Binaries: ${REPO_ROOT}/target/${BUILD_PROFILE}"
}

# Full entry point used by every platform script.
run_pipeline() {
    parse_build_args "$@"
    # Always runs, even with --skip-deps: this is where a platform exports the
    # PATH and PKG_CONFIG_PATH the build itself needs, not just the checks.
    if command -v prepare_platform >/dev/null 2>&1; then
        prepare_platform
    fi
    if [ "$RUN_DEPENDENCY_CHECK" = true ]; then
        run_setup
    fi
    if [ "$RUN_BUILD" = true ]; then
        run_build
        if command -v post_build_platform >/dev/null 2>&1; then
            post_build_platform
        fi
    fi
}

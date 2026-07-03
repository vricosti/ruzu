#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
bin="$repo_root/target/release/ruzu-cmd"
app="$repo_root/target/release/ruzu-cmd.app"
contents="$app/Contents"
macos="$contents/MacOS"
resources="$contents/Resources"
log_path="${RUZU_APP_LOG:-/tmp/ruzu_cmd_app.log}"
launch_mode="${RUZU_APP_LAUNCH_MODE:-open}"

if [[ ! -x "$bin" ]]; then
    echo "Missing $bin; build it first with cargo build --release --bin ruzu-cmd" >&2
    exit 1
fi

game_path="${1:-}"
if [[ -z "$game_path" ]]; then
    echo "usage: $0 /path/to/game.nsp [extra ruzu-cmd args...]" >&2
    exit 2
fi
shift

rm -rf "$app"
mkdir -p "$macos" "$resources"
cp "$bin" "$macos/ruzu-cmd"

cat > "$contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>en</string>
  <key>CFBundleExecutable</key>
  <string>ruzu-cmd</string>
  <key>CFBundleIdentifier</key>
  <string>org.ruzu.ruzu-cmd.dev</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundleName</key>
  <string>ruzu-cmd</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleShortVersionString</key>
  <string>0.0.0-dev</string>
  <key>CFBundleVersion</key>
  <string>0.0.0-dev</string>
  <key>LSMinimumSystemVersion</key>
  <string>11.0</string>
  <key>NSHighResolutionCapable</key>
  <true/>
  <key>NSSupportsAutomaticGraphicsSwitching</key>
  <true/>
</dict>
</plist>
PLIST

# LaunchServices may cache bundle metadata aggressively for reused paths.
/usr/bin/touch "$app"

rm -f "$log_path"
echo "Log: $log_path"
if [[ "$launch_mode" == "open" ]]; then
    echo "Starting $app through LaunchServices"
    open -n -W --env RUST_LOG="${RUST_LOG:-info}" \
        --stdout "$log_path" --stderr "$log_path" \
        "$app" --args -g "$game_path" "$@"
else
    (
        cd "$repo_root"
        exec "$macos/ruzu-cmd" -g "$game_path" "$@" >"$log_path" 2>&1
    ) &
    pid=$!
    echo "Started $app executable directly (pid $pid)"
    wait "$pid"
fi

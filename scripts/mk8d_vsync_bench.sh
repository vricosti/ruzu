#!/bin/bash
# Run MK8D for a fixed duration and extract max VSYNC before stall.
# Usage: mk8d_vsync_bench.sh <label> <duration_s>

set -u
LABEL="${1:?usage: $0 <label> <duration_s>}"
DURATION="${2:?usage: $0 <label> <duration_s>}"
LOGFILE="/tmp/mk8d-bench-${LABEL}.log"
ROM="/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"

# Clean any leftover processes
pkill -9 yuzu-cmd 2>/dev/null
sleep 1

rm -f "$LOGFILE"

env XDG_DATA_HOME=/tmp/ruzu-data \
    XDG_CACHE_HOME=/tmp/ruzu-cache \
    XDG_CONFIG_HOME=/tmp/ruzu-config \
    RUST_LOG=info \
    cargo run --bin yuzu-cmd -- -g "$ROM" > "$LOGFILE" 2>&1 &
CARGO_PID=$!

# Give it duration seconds to run
sleep "$DURATION"

# Kill the yuzu-cmd process spawned by cargo run
pkill -9 yuzu-cmd 2>/dev/null
sleep 2
wait $CARGO_PID 2>/dev/null

MAX_VSYNC=$(grep -oE 'woke #[0-9]+' "$LOGFILE" | grep -oE '[0-9]+' | sort -n | tail -1)
echo "[$LABEL] max_vsync=${MAX_VSYNC:-0}"

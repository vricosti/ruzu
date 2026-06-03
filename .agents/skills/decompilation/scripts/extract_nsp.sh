#!/bin/bash
# extract_nsp.sh — Extract Switch NSP to ExeFS (main + sdk + subsdk*)
# Usage: extract_nsp.sh <nsp_path> <out_dir>
#
# Steps:
#  1) hactool -t pfs0 → 5 NCAs
#  2) Identify the largest .nca (= program NCA)
#  3) hactool -t nca --exefsdir=<out>/exefs → main, rtld, sdk, subsdk*

set -euo pipefail

if [[ $# -lt 2 ]]; then
    echo "usage: $0 <nsp_path> <out_dir>" >&2
    exit 2
fi

NSP="$1"
OUT="$2"

if [[ ! -f "$NSP" ]]; then
    echo "error: NSP file not found: $NSP" >&2
    exit 1
fi

if ! command -v hactool >/dev/null; then
    echo "error: hactool not installed. apt install /home/vricosti/Dev/emulators/hactool_*.deb" >&2
    exit 1
fi

if [[ ! -e ~/.switch/prod.keys ]]; then
    echo "warning: ~/.switch/prod.keys missing; creating symlink from ruzu keys" >&2
    mkdir -p ~/.switch
    ln -sf ~/.local/share/ruzu/keys/prod.keys ~/.switch/prod.keys 2>/dev/null || true
    ln -sf ~/.local/share/ruzu/keys/title.keys ~/.switch/title.keys 2>/dev/null || true
fi

mkdir -p "$OUT/pfs0" "$OUT/exefs"

echo "[1/3] extract NSP → NCAs"
hactool -t pfs0 --pfs0dir="$OUT/pfs0" "$NSP" >/dev/null 2>&1
ls -la "$OUT/pfs0/"*.nca

echo
echo "[2/3] identify program NCA (largest)"
PROGRAM_NCA=$(ls -S "$OUT/pfs0/"*.nca | head -1)
echo "  → $PROGRAM_NCA"

echo
echo "[3/3] extract ExeFS"
hactool -t nca --exefsdir="$OUT/exefs" "$PROGRAM_NCA" 2>&1 | grep -vE "^\[WARN\]" | tail -5
echo
ls -la "$OUT/exefs/"
echo
echo "DONE — main NSO at: $OUT/exefs/main"

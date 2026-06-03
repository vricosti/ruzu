---
name: decompilation
description: Use this skill when investigating Nintendo Switch binaries (NSP/NCA/NSO) — extracting code via hactool, parsing NSO format (sections, dynsym, dynstr, JMPREL), identifying which symbol a PLT call address resolves to, computing the correct NSO offset from a runtime PC, or diffing a guest binary's behavior. Trigger phrases include "decompile", "extract NSO", "what symbol is at PC ...", "which function does this PLT call", "MK8D / Switch / NSP / NCA / NSO", "GOT entry", "find imports", "audio renderer addresses", "PLT[N]". Strongly recommended whenever an investigation needs to map runtime addresses to imported symbol names — manual offset math is error-prone (PLT layout has multiple correct candidates and ARM PC+8 traps).
---

# Decompilation skill — Nintendo Switch NSO

A toolbox for extracting and reverse-engineering Switch binaries (.NSP) so you can identify exactly which library function a runtime address resolves to.

## Critical lesson: ALWAYS verify the base address before computing offsets

ARM PLT analysis has 3 separate off-by-N traps:
- ARM `add ip, pc, #N` reads PC as `current_pc + 8` (pipeline)
- PLT[0] may be 4 bytes (single LDR) OR 20 bytes (standard stub) depending on toolchain
- NSO load base is NOT 0x200000 by default — it's determined at runtime and must be measured

**Never guess the base address.** Always verify by byte-pattern match: dump runtime bytes at a known PC, then locate those exact bytes inside the decompressed NSO sections. The base = `runtime_addr - found_offset`.

## Tools provided in `scripts/`

| Script | Purpose |
|---|---|
| `extract_nsp.sh <nsp_path> <out_dir>` | Run hactool to extract NSP → NCAs → ExeFS (main + sdk + subsdk*) |
| `nso_parse.py <nso_path>` | Print NSO header, sections, dynsym/dynstr/JMPREL info |
| `find_base.py <nso_path> <runtime_addr> <runtime_bytes_hex>` | Verify NSO load base by byte-pattern search |
| `find_symbol.py <nso_path> --pc <runtime_addr> --base <base>` | Given a runtime PC of a PLT call, find the resolved symbol name |
| `lookup_imports.py <nso_path> [filter]` | List imported functions (optional regex filter, e.g. "audio") |
| `disasm.py <bytes_hex> <vma>` | Capstone-disassemble bytes at VMA |

## Registry (`bases.json`)

Per-project file recording each NSO's measured load base. Keep this file in the **project root** (not in the skill dir) so it lives with the investigation. Example structure:

```json
{
  "MK8D 0100152000022000 v0": {
    "_notes": "Mario Kart 8 Deluxe — base addresses verified by byte-pattern match",
    "main":     "0x00206000",
    "_subsdk0": "TBD",
    "_sdk":     "TBD"
  }
}
```

The scripts accept `--base <hex>` directly OR `--game-key <key>` + `--registry <path>` to look up.

## Workflow: identify the symbol behind a PLT call

Given runtime PC of `bl <addr>` in the JIT trace:

```bash
# 1. Extract once (cache in /tmp or under project)
~/.agents/skills/decompilation/scripts/extract_nsp.sh "$NSP" /tmp/<game>_extract

# 2. Dump runtime bytes near a KNOWN PC via ruzu's RUZU_DUMP_MEM_AT_FIRST_SVC knob
#    (or any equivalent process-memory dumper). Get e.g. 16 bytes at the known PC.

# 3. Verify base — REQUIRED before any offset math
~/.agents/skills/decompilation/scripts/find_base.py \
    /tmp/<game>_extract/exefs/main 0x71EE40 "00009fe701a080e00a00a0e1f50215eb"
# → prints: base = 0x00206000

# 4. Look up the symbol behind the PLT call
~/.agents/skills/decompilation/scripts/find_symbol.py \
    /tmp/<game>_extract/exefs/main --pc 0xc6064c --base 0x00206000
# → prints: PLT call at runtime 0xc6064c → GOT slot 0xCCC6A4 → JMPREL[323] → sym[455]
#           = _ZN2nn5audio6AddAuxEPNS0_19AudioRendererConfigEPNS0_7AuxTypeE...
#           = nn::audio::AddAux(...)
```

## Workflow: identify imports by pattern

```bash
# List all `nn::audio::*` imports
~/.agents/skills/decompilation/scripts/lookup_imports.py \
    /tmp/<game>_extract/exefs/main 'nn::audio'
```

## When NOT to use this skill

- Reverse-engineering host-side (x86) code — use objdump directly
- Pure black-box behavioural debugging where the function name is already known
- Tool installation problems — call hactool directly first to confirm it works (`hactool` from `/usr/local/bin`)

## Dependencies

- `hactool` binary (apt install — see `/home/vricosti/Dev/emulators/hactool_*.deb` if reinstalling)
- Switch prod.keys at `~/.switch/prod.keys` (symlink from ruzu's keys location: `~/.local/share/ruzu/keys/prod.keys`)
- Python: `lz4`, `capstone` (install via `pip install --user --break-system-packages lz4 capstone` if missing)

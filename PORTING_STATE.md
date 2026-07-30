# Porting State

## 2026-07-31 — Windows game-list population

- Status: completed and verified.
- Interrupted slice: `ruzu/src/game_list.rs` directory selection, recursive
  scan, and metadata population.
- Confirmed behavior: recursive enumeration finds all nine `.xci` / `.nsp`
  candidates below the configured directory, but loader validation classifies
  every candidate as `FileType::Error`.
- Missing prerequisite: `core/src/crypto/key_manager.rs::resolve_keys_dir`
  claims to search legacy yuzu locations but does not include yuzu's actual
  Windows `%APPDATA%\yuzu\keys` directory. The available `prod.keys` and
  `title.keys` are therefore not loaded.
- Prerequisite result: the resolver now checks `%APPDATA%\yuzu\keys` and
  `%APPDATA%\suyu\keys` before the existing Unix-style fallbacks. The focused
  key-directory regression test passes and `DIFF.md` contains the required
  upstream comparison.
- Resumed work: make the directory toolbar select the newly added or sole
  directory so `Scan Subfolders` cannot silently remain disabled.
- Final verification: the rebuilt Windows GUI loaded the persisted recursive
  directory, remained responsive, and reported 7 games. The other 2 discovered
  NSP files are update-only packages and were skipped by the upstream
  `FileType::Error` rule.

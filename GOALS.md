# Active Goal

## fix-diff DIFF.md cleanup

Create or continue the `fix-diff` branch and process every `Unintentional differences (to fix)` and `Missing items` entry in `DIFF.md`.

For each item:

- First verify whether the item is still true by checking the matching upstream source under `/home/vricosti/Dev/emulators/zuyu/src/`.
- If the item is still valid, fix it in the Rust port while preserving upstream ownership, structure, constants, ordering, and behavior as closely as possible.
- If the item is stale or invalid, remove it from `DIFF.md`.
- Once an item is fixed, remove it from `DIFF.md`.
- Do not add new `DIFF.md` entries merely saying an item was fixed.

Completion requires all `Unintentional differences (to fix)` and `Missing items` entries in `DIFF.md` to be either fixed and removed, or verified stale and removed.

Status: active, not complete.

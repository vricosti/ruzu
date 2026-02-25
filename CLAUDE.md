# ruzu — Project Instructions

## Toolchain

Cargo is located at `/mnt/c/Users/smart/.cargo/bin/cargo.exe` (Windows binary accessed from WSL).

Always use the full path when running Rust commands:

```bash
/mnt/c/Users/smart/.cargo/bin/cargo.exe build --workspace
/mnt/c/Users/smart/.cargo/bin/cargo.exe test --workspace
/mnt/c/Users/smart/.cargo/bin/cargo.exe check --workspace
/mnt/c/Users/smart/.cargo/bin/cargo.exe clippy --workspace
```

## Git

Never add `Co-Authored-By` or any reference to Claude in commit messages.

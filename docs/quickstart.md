# ruzu quickstart

ruzu does not include Nintendo encryption keys, firmware, games, or other
copyrighted system files. Use only files dumped from hardware and games that
you own. This guide does not provide download links for those files.

## 1. Install decryption keys

1. Start `ruzu`.
2. Open **Tools > Install Decryption Keys**.
3. Select your dumped `prod.keys` file.

If `title.keys` or `key_retail.bin` are in the same directory, ruzu installs
them as well. A successful installation reloads the keys and rescans the game
list automatically.

## 2. Install firmware

Keys must be installed first.

1. Open **Tools > Install Firmware**.
2. Select the directory containing your dumped firmware `.nca` files.
3. Wait for the installation confirmation.

Firmware is required by system applets and by titles that depend on system
archives. The launcher can still open without firmware.

## 3. Add games

1. Select **Add Game Directory** in the launcher.
2. Choose the directory containing your dumped games.
3. If games are stored in nested directories, enable **Scan Subfolders** from
   the directory's context menu.
4. Double-click a listed title to start it.

If the directory remains empty, verify that the files use a supported format
such as NSP, XCI, NCA, NRO, or NSO and that the required keys are installed.

## Build and platform setup

See the repository [README](../README.md) for supported platforms, dependency
installation, build commands, and command-line usage.

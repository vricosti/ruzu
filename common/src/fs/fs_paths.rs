// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/common/fs/fs_paths.h
//! Directory path constants.

// ruzu data directories
pub const RUZU_DIR: &str = "ruzu";
pub const PORTABLE_DIR: &str = "user";

// Sub-directories contained within a ruzu data directory
pub const AMIIBO_DIR: &str = "amiibo";
pub const CACHE_DIR: &str = "cache";
pub const CONFIG_DIR: &str = "config";
pub const CRASH_DUMPS_DIR: &str = "crash_dumps";
pub const DUMP_DIR: &str = "dump";
pub const KEYS_DIR: &str = "keys";
pub const LOAD_DIR: &str = "load";
pub const LOG_DIR: &str = "log";
pub const NAND_DIR: &str = "nand";
pub const PLAY_TIME_DIR: &str = "play_time";
pub const SCREENSHOTS_DIR: &str = "screenshots";
pub const SDMC_DIR: &str = "sdmc";
pub const SHADER_DIR: &str = "shader";
pub const TAS_DIR: &str = "tas";
pub const ICONS_DIR: &str = "icons";

// ruzu-specific files
pub const LOG_FILE: &str = "ruzu_log.txt";

// SPDX-FileCopyrightText: Copyright 2025 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend_common/play_time_manager.{h,cpp}`.

use std::collections::BTreeMap;
use std::fs;
use std::io::{Read, Write};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{mpsc, Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use common::fs::path_util::{get_ruzu_path, RuzuPath};

type ProgramId = u64;
type PlayTime = u64;
type PlayTimeDatabase = BTreeMap<ProgramId, PlayTime>;

const PLAY_TIME_FILE: &str = "playtime.bin";
const AUTO_TIMESTAMP_INTERVAL: Duration = Duration::from_secs(30);
const PLAY_TIME_ELEMENT_SIZE: usize = std::mem::size_of::<u64>() * 2;

/// Eden's `PlayTimeElement`, serialized as two consecutive little-endian u64s.
#[repr(C)]
struct PlayTimeElement {
    program_id: ProgramId,
    play_time: PlayTime,
}

const _: () = assert!(std::mem::size_of::<PlayTimeElement>() == PLAY_TIME_ELEMENT_SIZE);

struct TimestampThread {
    stop: mpsc::Sender<()>,
    handle: JoinHandle<()>,
}

/// Tracks per-title play time and persists it in Eden's `playtime.bin` format.
pub struct PlayTimeManager {
    database: Arc<Mutex<PlayTimeDatabase>>,
    running_program_id: Arc<AtomicU64>,
    play_time_thread: Mutex<Option<TimestampThread>>,
}

impl PlayTimeManager {
    pub fn new() -> Self {
        let database = match read_play_time_file() {
            Ok(database) => database,
            Err(error) => {
                log::error!("Failed to read play time database: {error}");
                PlayTimeDatabase::new()
            }
        };
        Self {
            database: Arc::new(Mutex::new(database)),
            running_program_id: Arc::new(AtomicU64::new(0)),
            play_time_thread: Mutex::new(None),
        }
    }

    pub fn get_play_time(&self, program_id: u64) -> u64 {
        self.database
            .lock()
            .unwrap_or_else(|error| error.into_inner())
            .get(&program_id)
            .copied()
            .unwrap_or(0)
    }

    pub fn reset_program_play_time(&self, program_id: u64) {
        self.database
            .lock()
            .unwrap_or_else(|error| error.into_inner())
            .remove(&program_id);
        self.save();
    }

    pub fn set_program_id(&self, program_id: u64) {
        self.running_program_id.store(program_id, Ordering::Release);
    }

    pub fn set_play_time(&self, program_id: u64, play_time: u64) {
        self.database
            .lock()
            .unwrap_or_else(|error| error.into_inner())
            .insert(program_id, play_time);
        self.save();
    }

    pub fn start(&self) {
        self.stop();

        let database = Arc::clone(&self.database);
        let running_program_id = Arc::clone(&self.running_program_id);
        let (stop, stopped) = mpsc::channel();
        let handle = thread::Builder::new()
            .name("PlayTimeReport".to_owned())
            .spawn(move || auto_timestamp(database, running_program_id, stopped))
            .expect("failed to start play-time reporting thread");
        *self
            .play_time_thread
            .lock()
            .unwrap_or_else(|error| error.into_inner()) = Some(TimestampThread { stop, handle });
    }

    pub fn stop(&self) {
        let thread = self
            .play_time_thread
            .lock()
            .unwrap_or_else(|error| error.into_inner())
            .take();
        if let Some(thread) = thread {
            let _ = thread.stop.send(());
            if let Err(error) = thread.handle.join() {
                log::error!("Play-time reporting thread panicked: {error:?}");
            }
        }
    }

    pub fn get_readable_play_time(time_seconds: u64) -> String {
        if time_seconds == 0 {
            String::new()
        } else {
            format!(
                "{:02}:{:02}:{:02}",
                time_seconds / 3600,
                (time_seconds / 60) % 60,
                time_seconds % 60
            )
        }
    }

    pub fn get_play_time_hours(time_seconds: u64) -> String {
        (time_seconds / 3600).to_string()
    }

    pub fn get_play_time_minutes(time_seconds: u64) -> String {
        ((time_seconds % 3600) / 60).to_string()
    }

    pub fn get_play_time_seconds(time_seconds: u64) -> String {
        (time_seconds % 60).to_string()
    }

    fn save(&self) {
        let database = self
            .database
            .lock()
            .unwrap_or_else(|error| error.into_inner());
        if let Err(error) = write_play_time_file(&database) {
            log::error!("Failed to update play time database: {error}");
        }
    }
}

impl Default for PlayTimeManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for PlayTimeManager {
    fn drop(&mut self) {
        self.stop();
        self.save();
    }
}

fn play_time_path() -> std::path::PathBuf {
    get_ruzu_path(RuzuPath::PlayTimeDir).join(PLAY_TIME_FILE)
}

fn read_play_time_file() -> std::io::Result<PlayTimeDatabase> {
    let path = play_time_path();
    if !path.exists() {
        return Ok(PlayTimeDatabase::new());
    }
    let mut bytes = Vec::new();
    fs::File::open(path)?.read_to_end(&mut bytes)?;
    Ok(decode_play_time_database(&bytes))
}

fn write_play_time_file(database: &PlayTimeDatabase) -> std::io::Result<()> {
    let path = play_time_path();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::File::create(path)?.write_all(&encode_play_time_database(database))
}

fn decode_play_time_database(bytes: &[u8]) -> PlayTimeDatabase {
    let mut database = PlayTimeDatabase::new();
    for element in bytes.chunks_exact(PLAY_TIME_ELEMENT_SIZE) {
        let program_id = u64::from_le_bytes(element[..8].try_into().unwrap());
        let play_time = u64::from_le_bytes(element[8..16].try_into().unwrap());
        if program_id != 0 {
            database.insert(program_id, play_time);
        }
    }
    database
}

fn encode_play_time_database(database: &PlayTimeDatabase) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(database.len() * PLAY_TIME_ELEMENT_SIZE);
    for (&program_id, &play_time) in database {
        if program_id == 0 {
            continue;
        }
        bytes.extend_from_slice(&program_id.to_le_bytes());
        bytes.extend_from_slice(&play_time.to_le_bytes());
    }
    bytes
}

fn auto_timestamp(
    database: Arc<Mutex<PlayTimeDatabase>>,
    running_program_id: Arc<AtomicU64>,
    stop: mpsc::Receiver<()>,
) {
    let mut timestamp = Instant::now();
    loop {
        let stopping = match stop.recv_timeout(AUTO_TIMESTAMP_INTERVAL) {
            Ok(()) | Err(mpsc::RecvTimeoutError::Disconnected) => true,
            Err(mpsc::RecvTimeoutError::Timeout) => false,
        };

        let now = Instant::now();
        let duration = now.duration_since(timestamp).as_secs();
        timestamp = now;
        let program_id = running_program_id.load(Ordering::Acquire);
        *database
            .lock()
            .unwrap_or_else(|error| error.into_inner())
            .entry(program_id)
            .or_default() += duration;
        if let Err(error) = write_play_time_file(
            &database
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner()),
        ) {
            log::error!("Failed to update play time database: {error}");
        }

        if stopping {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn readable_play_time_matches_eden() {
        assert_eq!(PlayTimeManager::get_readable_play_time(0), "");
        assert_eq!(PlayTimeManager::get_readable_play_time(1), "00:00:01");
        assert_eq!(PlayTimeManager::get_readable_play_time(3_725), "01:02:05");
    }

    #[test]
    fn database_format_is_two_u64_words_per_entry() {
        let database = BTreeMap::from([(0, 99), (0x0100_1234_5678_9000, 3_725)]);
        let bytes = encode_play_time_database(&database);
        assert_eq!(bytes.len(), 16);
        assert_eq!(
            decode_play_time_database(&bytes),
            BTreeMap::from([(0x0100_1234_5678_9000, 3_725)])
        );
    }

    #[test]
    fn truncated_tail_is_ignored_like_upstream_element_count() {
        let mut bytes = encode_play_time_database(&BTreeMap::from([(7, 12)]));
        bytes.extend_from_slice(&[0xAA; 7]);
        assert_eq!(decode_play_time_database(&bytes), BTreeMap::from([(7, 12)]));
    }
}

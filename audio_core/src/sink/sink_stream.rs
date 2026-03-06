use crate::common::common::{MAX_CHANNELS, TARGET_SAMPLE_COUNT, TARGET_SAMPLE_RATE};
use crate::SharedSystem;
use std::cmp::min;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::sync::{Condvar, Mutex as StdMutex};
use std::time::{Duration, Instant};

use parking_lot::Mutex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StreamType {
    Render,
    Out,
    In,
}

#[derive(Debug, Clone, Default)]
pub struct SinkBuffer {
    pub frames: u64,
    pub frames_played: u64,
    pub tag: u64,
    pub consumed: bool,
}

pub type SinkStreamHandle = Arc<Mutex<SinkStream>>;

pub struct SinkStream {
    pub system: SharedSystem,
    pub stream_type: StreamType,
    pub system_channels: u32,
    pub device_channels: u32,
    pub paused: bool,
    pub name: String,
    queue: VecDeque<SinkBuffer>,
    playing_buffer: SinkBuffer,
    samples: VecDeque<i16>,
    last_frame: [i16; MAX_CHANNELS],
    queued_buffers: u32,
    max_queue_size: u32,
    min_played_sample_count: u64,
    max_played_sample_count: u64,
    last_sample_count_update_time: Instant,
    system_volume: f32,
    device_volume: f32,
    release_cv: Condvar,
    release_mutex: StdMutex<()>,
}

impl SinkStream {
    pub fn new(system: SharedSystem, stream_type: StreamType) -> Self {
        Self {
            system,
            stream_type,
            system_channels: 2,
            device_channels: 2,
            paused: true,
            name: String::new(),
            queue: VecDeque::new(),
            playing_buffer: SinkBuffer {
                consumed: true,
                ..SinkBuffer::default()
            },
            samples: VecDeque::new(),
            last_frame: [0; MAX_CHANNELS],
            queued_buffers: 0,
            max_queue_size: 0,
            min_played_sample_count: 0,
            max_played_sample_count: 0,
            last_sample_count_update_time: Instant::now(),
            system_volume: 1.0,
            device_volume: 1.0,
            release_cv: Condvar::new(),
            release_mutex: StdMutex::new(()),
        }
    }

    pub fn finalize(&mut self) {}

    pub fn start(&mut self, _resume: bool) {
        self.paused = false;
    }

    pub fn stop(&mut self) {
        self.signal_pause();
    }

    pub fn is_paused(&self) -> bool {
        self.paused
    }

    pub fn get_system_channels(&self) -> u32 {
        self.system_channels
    }

    pub fn set_system_channels(&mut self, channels: u32) {
        self.system_channels = channels;
    }

    pub fn get_device_channels(&self) -> u32 {
        self.device_channels
    }

    pub fn get_system_volume(&self) -> f32 {
        self.system_volume
    }

    pub fn get_device_volume(&self) -> f32 {
        self.device_volume
    }

    pub fn set_system_volume(&mut self, volume: f32) {
        self.system_volume = volume;
    }

    pub fn set_device_volume(&mut self, volume: f32) {
        self.device_volume = volume;
    }

    pub fn get_queue_size(&self) -> u32 {
        self.queued_buffers
    }

    pub fn set_ring_size(&mut self, ring_size: u32) {
        self.max_queue_size = ring_size;
    }

    pub fn append_buffer(&mut self, mut buffer: SinkBuffer, samples: &[i16]) {
        let queued_buffer = {
            buffer.consumed = false;
            buffer.frames_played = 0;
            buffer
        };

        if self.stream_type == StreamType::In {
            self.queue.push_back(queued_buffer);
            self.queued_buffers += 1;
            return;
        }

        let volume = self.system_volume * self.device_volume;
        if self.system_channels == 6 && self.device_channels == 2 {
            // Match yuzu's 6ch->2ch sink downmix.
            const DOWN_MIX_COEFF: [f32; 4] = [1.0, 0.596, 0.354, 0.707];

            for frame in samples.chunks_exact(self.system_channels as usize) {
                let fl = frame[0] as f32;
                let fr = frame[1] as f32;
                let c = frame[2] as f32;
                let lfe = frame[3] as f32;
                let bl = frame[4] as f32;
                let br = frame[5] as f32;

                let left = (fl * DOWN_MIX_COEFF[0]
                    + c * DOWN_MIX_COEFF[1]
                    + lfe * DOWN_MIX_COEFF[2]
                    + bl * DOWN_MIX_COEFF[3])
                    * volume;
                let right = (fr * DOWN_MIX_COEFF[0]
                    + c * DOWN_MIX_COEFF[1]
                    + lfe * DOWN_MIX_COEFF[2]
                    + br * DOWN_MIX_COEFF[3])
                    * volume;

                self.samples.push_back(clamp_i16(left));
                self.samples.push_back(clamp_i16(right));
            }
        } else if self.system_channels == 2 && self.device_channels == 6 {
            // Match yuzu's current passthrough-style 2ch->6ch expansion.
            for frame in samples.chunks_exact(self.system_channels as usize) {
                self.samples.push_back(clamp_i16(frame[0] as f32 * volume));
                self.samples.push_back(clamp_i16(frame[1] as f32 * volume));
                self.samples.push_back(0);
                self.samples.push_back(0);
                self.samples.push_back(0);
                self.samples.push_back(0);
            }
        } else {
            for sample in samples {
                self.samples.push_back(clamp_i16(*sample as f32 * volume));
            }
        }
        self.queue.push_back(queued_buffer);
        self.queued_buffers += 1;
    }

    pub fn release_buffer(&mut self, num_samples: u64) -> Vec<i16> {
        let count = min(num_samples as usize, self.samples.len());
        let mut out = Vec::with_capacity(num_samples as usize);
        for _ in 0..count {
            out.push(self.samples.pop_front().unwrap_or_default());
        }
        let volume = if self.stream_type == StreamType::In {
            self.system_volume * self.device_volume * 8.0
        } else {
            1.0
        };
        if volume != 1.0 {
            for sample in &mut out {
                *sample = clamp_i16(*sample as f32 * volume);
            }
        }
        if out.len() < num_samples as usize {
            out.resize(num_samples as usize, 0);
        }
        out
    }

    pub fn clear_queue(&mut self) {
        self.samples.clear();
        self.queue.clear();
        self.queued_buffers = 0;
        self.playing_buffer = SinkBuffer {
            consumed: true,
            ..SinkBuffer::default()
        };
        self.release_cv.notify_one();
    }

    pub fn get_expected_played_sample_count(&self) -> u64 {
        let elapsed = self.last_sample_count_update_time.elapsed();
        let expected_delta = (TARGET_SAMPLE_RATE as u128 * elapsed.as_nanos()) / 1_000_000_000u128;
        self.min_played_sample_count
            .saturating_add(expected_delta as u64)
            .min(self.max_played_sample_count)
            .saturating_add(TARGET_SAMPLE_COUNT as u64 * 3)
    }

    pub fn wait_free_space(&self) {
        static NOT_STOPPED: AtomicBool = AtomicBool::new(false);
        self.wait_free_space_with_stop(&NOT_STOPPED);
    }

    pub fn wait_free_space_with_stop(&self, stop_requested: &AtomicBool) {
        if self.max_queue_size == 0 {
            return;
        }

        let mut guard = self
            .release_mutex
            .lock()
            .expect("sink stream release mutex poisoned");

        while !self.paused
            && self.queued_buffers >= self.max_queue_size
            && !stop_requested.load(Ordering::SeqCst)
        {
            let (new_guard, _) = self
                .release_cv
                .wait_timeout(guard, Duration::from_millis(5))
                .expect("sink stream condvar wait poisoned");
            guard = new_guard;

            if self.paused
                || stop_requested.load(Ordering::SeqCst)
                || self.queued_buffers < self.max_queue_size
            {
                break;
            }

            while !self.paused
                && self.queued_buffers > self.max_queue_size + 3
                && !stop_requested.load(Ordering::SeqCst)
            {
                let (new_guard, _) = self
                    .release_cv
                    .wait_timeout(guard, Duration::from_millis(5))
                    .expect("sink stream condvar wait poisoned");
                guard = new_guard;
            }
        }
    }

    pub fn process_audio_in(&mut self, input_buffer: &[i16], num_frames: usize) {
        if self.system.lock().is_paused() || self.system.lock().is_shutting_down() {
            return;
        }

        let frame_size = self.device_channels as usize;
        let frame_size = frame_size.max(1);
        let frame_size_bytes = frame_size.min(MAX_CHANNELS) * std::mem::size_of::<i16>();
        let mut frames_written = 0usize;

        while frames_written < num_frames {
            if self.playing_buffer.consumed || self.playing_buffer.frames == 0 {
                let Some(buffer) = self.queue.pop_front() else {
                    for sample in input_buffer.iter().skip(frames_written * frame_size) {
                        self.samples.push_back(*sample);
                    }
                    let last_frame_start = (num_frames.saturating_sub(1)) * frame_size;
                    let available = input_buffer.len().saturating_sub(last_frame_start);
                    let copy_len = available.min(MAX_CHANNELS);
                    if copy_len > 0 {
                        self.last_frame[..copy_len].copy_from_slice(
                            &input_buffer[last_frame_start..last_frame_start + copy_len],
                        );
                    }
                    return;
                };
                self.playing_buffer = buffer;
                self.queued_buffers = self.queued_buffers.saturating_sub(1);
                self.release_cv.notify_one();
            }

            let frames_available = (self.playing_buffer.frames - self.playing_buffer.frames_played)
                .min((num_frames - frames_written) as u64)
                as usize;
            let sample_start = frames_written * frame_size;
            let sample_end = sample_start + frames_available * frame_size;
            for sample in &input_buffer[sample_start..sample_end] {
                self.samples.push_back(*sample);
            }

            frames_written += frames_available;
            self.playing_buffer.frames_played += frames_available as u64;
            if self.playing_buffer.frames_played >= self.playing_buffer.frames {
                self.playing_buffer.consumed = true;
            }
        }

        if num_frames > 0 && frame_size_bytes > 0 {
            let last_frame_start = (num_frames - 1) * frame_size;
            let copy_len = frame_size.min(MAX_CHANNELS);
            self.last_frame[..copy_len]
                .copy_from_slice(&input_buffer[last_frame_start..last_frame_start + copy_len]);
        }
    }

    pub fn process_audio_out_and_render(&mut self, output_buffer: &mut [i16], num_frames: usize) {
        if self.system.lock().is_paused() || self.system.lock().is_shutting_down() {
            if self.system.lock().is_shutting_down() {
                self.queued_buffers = 0;
                self.release_cv.notify_one();
            }
            output_buffer.fill(0);
            return;
        }

        let frame_size = self.device_channels as usize;
        let frame_size = frame_size.max(1);
        let mut frames_written = 0usize;
        let mut actual_frames_written = 0usize;

        while frames_written < num_frames {
            if self.playing_buffer.consumed || self.playing_buffer.frames == 0 {
                let Some(buffer) = self.queue.pop_front() else {
                    for frame in frames_written..num_frames {
                        let base = frame * frame_size;
                        let copy_len = frame_size.min(MAX_CHANNELS);
                        output_buffer[base..base + copy_len]
                            .copy_from_slice(&self.last_frame[..copy_len]);
                    }
                    break;
                };
                self.playing_buffer = buffer;
                self.queued_buffers = self.queued_buffers.saturating_sub(1);
                self.release_cv.notify_one();
            }

            let frames_available = (self.playing_buffer.frames - self.playing_buffer.frames_played)
                .min((num_frames - frames_written) as u64)
                as usize;
            let samples_to_pop = frames_available * frame_size;
            let base = frames_written * frame_size;
            for sample in &mut output_buffer[base..base + samples_to_pop] {
                *sample = self.samples.pop_front().unwrap_or(self.last_frame[0]);
            }

            frames_written += frames_available;
            actual_frames_written += frames_available;
            self.playing_buffer.frames_played += frames_available as u64;
            if self.playing_buffer.frames_played >= self.playing_buffer.frames {
                self.playing_buffer.consumed = true;
            }
        }

        if num_frames > 0 {
            let last_frame_start = (num_frames - 1) * frame_size;
            let copy_len = frame_size.min(MAX_CHANNELS);
            self.last_frame[..copy_len]
                .copy_from_slice(&output_buffer[last_frame_start..last_frame_start + copy_len]);
        }

        self.last_sample_count_update_time = Instant::now();
        self.min_played_sample_count = self.max_played_sample_count;
        self.max_played_sample_count = self
            .max_played_sample_count
            .saturating_add(actual_frames_written as u64);
    }

    pub fn signal_pause(&mut self) {
        self.paused = true;
        self.release_cv.notify_one();
    }
}

fn clamp_i16(sample: f32) -> i16 {
    sample.clamp(i16::MIN as f32, i16::MAX as f32) as i16
}

#[cfg(test)]
mod tests {
    use super::*;
    use parking_lot::Mutex;

    fn make_system() -> SharedSystem {
        Arc::new(Mutex::new(ruzu_core::core::System::new()))
    }

    #[test]
    fn expected_played_sample_count_is_tracked_in_frames() {
        let system = make_system();
        let mut stream = SinkStream::new(system, StreamType::Out);
        stream.device_channels = 2;
        stream.system_channels = 2;
        stream.append_buffer(
            SinkBuffer {
                frames: 2,
                frames_played: 0,
                tag: 1,
                consumed: false,
            },
            &[1, 2, 3, 4],
        );

        let mut output = [0i16; 4];
        stream.process_audio_out_and_render(&mut output, 2);

        assert_eq!(output, [1, 2, 3, 4]);
        assert!(stream.get_expected_played_sample_count() >= 2);
        assert!(stream.get_expected_played_sample_count() < 2 + TARGET_SAMPLE_COUNT as u64 * 4);
    }

    #[test]
    fn process_audio_in_consumes_queued_input_buffer_frames() {
        let system = make_system();
        let mut stream = SinkStream::new(system, StreamType::In);
        stream.device_channels = 2;
        stream.append_buffer(
            SinkBuffer {
                frames: 2,
                frames_played: 0,
                tag: 7,
                consumed: false,
            },
            &[],
        );

        stream.process_audio_in(&[10, 11, 12, 13], 2);

        assert_eq!(stream.get_queue_size(), 0);
        assert_eq!(stream.release_buffer(4), vec![80, 88, 96, 104]);
    }

    #[test]
    fn append_buffer_downmixes_six_channels_to_two() {
        let system = make_system();
        let mut stream = SinkStream::new(system, StreamType::Render);
        stream.system_channels = 6;
        stream.device_channels = 2;

        stream.append_buffer(
            SinkBuffer {
                frames: 1,
                frames_played: 0,
                tag: 1,
                consumed: false,
            },
            &[100, 200, 300, 400, 500, 600],
        );

        assert_eq!(stream.release_buffer(2), vec![773, 944]);
    }

    #[test]
    fn append_buffer_expands_two_channels_to_six() {
        let system = make_system();
        let mut stream = SinkStream::new(system, StreamType::Render);
        stream.system_channels = 2;
        stream.device_channels = 6;

        stream.append_buffer(
            SinkBuffer {
                frames: 1,
                frames_played: 0,
                tag: 1,
                consumed: false,
            },
            &[10, 20],
        );

        assert_eq!(stream.release_buffer(6), vec![10, 20, 0, 0, 0, 0]);
    }

    #[test]
    fn release_buffer_applies_audio_in_gain() {
        let system = make_system();
        let mut stream = SinkStream::new(system, StreamType::In);
        stream.samples.extend([10, -20]);

        assert_eq!(stream.release_buffer(2), vec![80, -160]);
    }
}

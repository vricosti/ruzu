// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/clustering_processor.h and clustering_processor.cpp

use std::collections::VecDeque;

use super::irs_types::*;
use super::processor_base::ProcessorBase;

const WIDTH: usize = 320;
const HEIGHT: usize = 240;
const FORMAT: ImageTransferProcessorFormat = ImageTransferProcessorFormat::Size320x240;

/// This is nn::irsensor::ClusteringProcessorConfig (unpacked)
#[derive(Debug, Clone, Copy)]
pub struct ClusteringProcessorConfig {
    pub camera_config: CameraConfig,
    pub window_of_interest: IrsRect,
    pub pixel_count_min: u32,
    pub pixel_count_max: u32,
    pub object_intensity_min: u32,
    pub is_external_light_filter_enabled: bool,
}

impl Default for ClusteringProcessorConfig {
    fn default() -> Self {
        Self {
            camera_config: CameraConfig {
                exposure_time: 200_000, // 200ms in microseconds
                light_target: CameraLightTarget::BrightLeds,
                gain: 2,
                is_negative_used: false,
                _padding: [0; 7],
            },
            window_of_interest: IrsRect {
                x: 0,
                y: 0,
                width: WIDTH as i16,
                height: HEIGHT as i16,
            },
            pixel_count_min: 3,
            pixel_count_max: (WIDTH * HEIGHT) as u32,
            object_intensity_min: 150,
            is_external_light_filter_enabled: true,
        }
    }
}

/// This is nn::irsensor::ClusteringData
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ClusteringData {
    pub average_intensity: f32,
    pub centroid: IrsCentroid,
    pub pixel_count: u32,
    pub bound: IrsRect,
}
const _: () = assert!(std::mem::size_of::<ClusteringData>() == 0x18);

/// This is nn::irsensor::ClusteringProcessorState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ClusteringProcessorState {
    pub sampling_number: i64,
    pub timestamp: u64,
    pub object_count: u8,
    pub _padding: [u8; 3],
    pub ambient_noise_level: CameraAmbientNoiseLevel,
    pub data: [ClusteringData; 0x10],
}

/// Clustering processor — groups bright pixels in the IR camera image into
/// clusters and reports their properties. Full port of the upstream BFS-based
/// clustering algorithm.
pub struct ClusteringProcessor {
    pub base: ProcessorBase,
    current_config: ClusteringProcessorConfig,
    next_state: ClusteringProcessorState,
}

impl ClusteringProcessor {
    pub fn new(device: &mut DeviceFormat) -> Self {
        device.mode = IrSensorMode::ClusteringProcessor;
        device.camera_status = IrCameraStatus::Unconnected;
        device.camera_internal_status = IrCameraInternalStatus::Stopped;
        Self {
            base: ProcessorBase::new(),
            current_config: ClusteringProcessorConfig::default(),
            next_state: ClusteringProcessorState::default(),
        }
    }

    pub fn start_processor(&mut self) {
        // Upstream sets Available/Ready
    }

    pub fn suspend_processor(&mut self) {}
    pub fn stop_processor(&mut self) {}

    fn get_pixel(data: &[u8], x: usize, y: usize) -> u8 {
        let idx = (y * WIDTH) + x;
        if idx >= data.len() {
            0
        } else {
            data[idx]
        }
    }

    fn set_pixel(data: &mut [u8], x: usize, y: usize, value: u8) {
        let idx = (y * WIDTH) + x;
        if idx < data.len() {
            data[idx] = value;
        }
    }

    fn get_pixel_properties(data: &[u8], x: usize, y: usize) -> ClusteringData {
        ClusteringData {
            average_intensity: Self::get_pixel(data, x, y) as f32 / 255.0,
            centroid: IrsCentroid {
                x: x as f32,
                y: y as f32,
            },
            pixel_count: 1,
            bound: IrsRect {
                x: x as i16,
                y: y as i16,
                width: 1,
                height: 1,
            },
        }
    }

    fn merge_cluster(a: &ClusteringData, b: &ClusteringData) -> ClusteringData {
        let a_pc = a.pixel_count as f32;
        let b_pc = b.pixel_count as f32;
        let pixel_count = a_pc + b_pc;
        let average_intensity =
            (a.average_intensity * a_pc + b.average_intensity * b_pc) / pixel_count;
        let centroid = IrsCentroid {
            x: (a.centroid.x * a_pc + b.centroid.x * b_pc) / pixel_count,
            y: (a.centroid.y * a_pc + b.centroid.y * b_pc) / pixel_count,
        };

        let bound_start_x = a.bound.x.min(b.bound.x);
        let bound_start_y = a.bound.y.min(b.bound.y);
        let a_end_x = a.bound.x + a.bound.width;
        let a_end_y = a.bound.y + a.bound.height;
        let b_end_x = b.bound.x + b.bound.width;
        let b_end_y = b.bound.y + b.bound.height;

        ClusteringData {
            average_intensity,
            centroid,
            pixel_count: pixel_count as u32,
            bound: IrsRect {
                x: bound_start_x,
                y: bound_start_y,
                width: if a_end_x > b_end_x {
                    a_end_x - bound_start_x
                } else {
                    b_end_x - bound_start_x
                },
                height: if a_end_y > b_end_y {
                    a_end_y - bound_start_y
                } else {
                    b_end_y - bound_start_y
                },
            },
        }
    }

    /// Port of ClusteringProcessor::GetClusterProperties — BFS flood-fill.
    fn get_cluster_properties(&self, data: &mut [u8], x: usize, y: usize) -> ClusteringData {
        let mut search_points: VecDeque<(usize, usize)> = VecDeque::new();
        let mut current_cluster = Self::get_pixel_properties(data, x, y);
        Self::set_pixel(data, x, y, 0);
        search_points.push_back((x, y));

        while let Some((px, py)) = search_points.pop_front() {
            if px == 0 || py == 0 {
                continue;
            }

            let new_points = [
                (px.wrapping_sub(1), py),
                (px, py.wrapping_sub(1)),
                (px + 1, py),
                (px, py + 1),
            ];

            for &(nx, ny) in &new_points {
                if nx >= WIDTH || ny >= HEIGHT {
                    continue;
                }
                if (Self::get_pixel(data, nx, ny) as u32) < self.current_config.object_intensity_min
                {
                    continue;
                }
                let cluster = Self::get_pixel_properties(data, nx, ny);
                current_cluster = Self::merge_cluster(&current_cluster, &cluster);
                Self::set_pixel(data, nx, ny, 0);
                search_points.push_back((nx, ny));
            }
        }

        current_cluster
    }

    fn remove_low_intensity_data(&self, data: &mut [u8]) {
        for pixel in data.iter_mut() {
            if (*pixel as u32) < self.current_config.pixel_count_min {
                *pixel = 0;
            }
        }
    }

    /// Process camera data into clustering results.
    pub fn on_controller_update(&mut self, camera_data: &[u8], sample: i64, timestamp: u64) {
        self.next_state = ClusteringProcessorState::default();
        let mut filtered_image = camera_data.to_vec();

        self.remove_low_intensity_data(&mut filtered_image);

        let window_start_x = self.current_config.window_of_interest.x as usize;
        let window_start_y = self.current_config.window_of_interest.y as usize;
        let window_end_x = window_start_x + self.current_config.window_of_interest.width as usize;
        let window_end_y = window_start_y + self.current_config.window_of_interest.height as usize;

        for y in window_start_y..window_end_y {
            for x in window_start_x..window_end_x {
                let pixel = Self::get_pixel(&filtered_image, x, y);
                if pixel == 0 {
                    continue;
                }
                let cluster = self.get_cluster_properties(&mut filtered_image, x, y);
                if cluster.pixel_count > self.current_config.pixel_count_max {
                    continue;
                }
                if cluster.pixel_count < self.current_config.pixel_count_min {
                    continue;
                }
                if (self.next_state.object_count as usize) >= self.next_state.data.len() {
                    continue;
                }
                self.next_state.data[self.next_state.object_count as usize] = cluster;
                self.next_state.object_count += 1;
            }
        }

        self.next_state.sampling_number = sample;
        self.next_state.timestamp = timestamp;
        self.next_state.ambient_noise_level = CameraAmbientNoiseLevel::Low;
    }

    /// Port of ClusteringProcessor::SetConfig.
    pub fn set_config(&mut self, config: PackedClusteringProcessorConfig) {
        self.current_config.camera_config.exposure_time = config.camera_config.exposure_time;
        self.current_config.camera_config.gain = config.camera_config.gain as u32;
        self.current_config.camera_config.is_negative_used = config.camera_config.is_negative_used;
        self.current_config.camera_config.light_target = unsafe {
            std::mem::transmute::<u32, CameraLightTarget>(config.camera_config.light_target as u32)
        };
        self.current_config.window_of_interest = config.window_of_interest;
        self.current_config.pixel_count_min = config.pixel_count_min;
        self.current_config.pixel_count_max = config.pixel_count_max;
        self.current_config.is_external_light_filter_enabled =
            config.is_external_light_filter_enabled;
        self.current_config.object_intensity_min = config.object_intensity_min as u32;
    }
}

use crate::sink::sink::{new_stream_handle, Sink};
use crate::sink::sink_stream::{SinkStream, SinkStreamHandle, StreamType};
use crate::SharedSystem;
use std::sync::Arc;

struct StreamEntry {
    name: String,
    stream_type: StreamType,
    handle: SinkStreamHandle,
}

#[derive(Default)]
pub struct NullSink {
    streams: Vec<StreamEntry>,
    device_volume: f32,
    system_volume: f32,
    device_channels: u32,
    system_channels: u32,
}

impl NullSink {
    pub fn new(_device_id: &str) -> Self {
        Self {
            streams: Vec::new(),
            device_volume: 1.0,
            system_volume: 1.0,
            device_channels: 2,
            system_channels: 2,
        }
    }
}

impl Sink for NullSink {
    fn close_stream(&mut self, stream: &SinkStreamHandle) {
        self.streams
            .retain(|entry| !Arc::ptr_eq(&entry.handle, stream));
    }

    fn close_streams(&mut self) {
        self.streams.clear();
    }

    fn acquire_sink_stream(
        &mut self,
        system: SharedSystem,
        system_channels: u32,
        name: &str,
        stream_type: StreamType,
    ) -> SinkStreamHandle {
        if let Some(existing) = self
            .streams
            .iter()
            .find(|entry| entry.name == name && entry.stream_type == stream_type)
        {
            return existing.handle.clone();
        }

        let mut stream = SinkStream::new(system, stream_type);
        stream.system_channels = system_channels;
        stream.device_channels = self.device_channels;
        stream.name = name.to_string();
        stream.set_device_volume(self.device_volume);
        stream.set_system_volume(self.system_volume);
        let handle = new_stream_handle(stream);
        self.streams.push(StreamEntry {
            name: name.to_string(),
            stream_type,
            handle: handle.clone(),
        });
        handle
    }

    fn get_device_volume(&self) -> f32 {
        self.device_volume
    }

    fn set_device_volume(&mut self, volume: f32) {
        self.device_volume = volume;
    }

    fn set_system_volume(&mut self, volume: f32) {
        self.system_volume = volume;
    }

    fn get_device_channels(&self) -> u32 {
        self.device_channels
    }

    fn get_system_channels(&self) -> u32 {
        self.system_channels
    }
}

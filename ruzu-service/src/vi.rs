// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `vi:m` -- Visual Interface service and sub-interfaces.
//!
//! The VI service chain:
//! 1. `vi:m` → `GetDisplayService(2)` → returns move handle to sub-interface
//! 2. `vi:IApplicationDisplayService` → display management commands
//! 3. `vi:IHOSBinderDriver` → Binder/BufferQueue operations via Parcel
//!
//! This module provides three ServiceHandler implementations:
//! - `ViManagerService` — the root `vi:m` interface
//! - `ViDisplayService` — display management (OpenDisplay, CreateStrayLayer, etc.)
//! - `ViBinderService` — Binder transact for buffer queue operations

use std::sync::Arc;

use parking_lot::RwLock;
use ruzu_gpu::gpu_context::GpuContext;

use crate::buffer_queue::{BufferQueue, GraphicBuffer};
use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use crate::parcel::Parcel;

// ── vi:m (ViManagerService) ─────────────────────────────────────────────────

/// Root `vi:m` service. Returns a move handle to the display sub-interface.
pub struct ViManagerService;

impl ViManagerService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ViManagerService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for ViManagerService {
    fn service_name(&self) -> &str {
        "vi:m"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("vi:m: cmd_id={}", cmd_id);

        match cmd_id {
            // GetDisplayService (2): returns a move handle to IApplicationDisplayService.
            // The IPC bridge will create a KClientSession for "vi:IApplicationDisplayService".
            2 => {
                log::info!("vi:m: GetDisplayService");
                // The move handle is a placeholder — the IPC bridge detects this
                // response and creates the actual session.
                IpcResponse::success().with_move_handle(0)
            }

            _ => {
                log::warn!("vi:m: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── IApplicationDisplayService (ViDisplayService) ───────────────────────────

/// Display management sub-interface.
pub struct ViDisplayService {
    buffer_queue: Arc<RwLock<BufferQueue>>,
    /// Vsync event handle, set during initialization.
    vsync_event_handle: Option<u32>,
    /// Display ID counter.
    next_display_id: u64,
    /// Layer ID counter.
    next_layer_id: u64,
}

impl ViDisplayService {
    pub fn new(buffer_queue: Arc<RwLock<BufferQueue>>) -> Self {
        Self {
            buffer_queue,
            vsync_event_handle: None,
            next_display_id: 1,
            next_layer_id: 1,
        }
    }

    /// Create with a pre-allocated vsync kernel event handle.
    pub fn new_with_vsync_event(buffer_queue: Arc<RwLock<BufferQueue>>, handle: u32) -> Self {
        Self {
            buffer_queue,
            vsync_event_handle: Some(handle),
            next_display_id: 1,
            next_layer_id: 1,
        }
    }

    /// Set the vsync event handle (created by the main crate).
    pub fn set_vsync_event_handle(&mut self, handle: u32) {
        self.vsync_event_handle = Some(handle);
    }
}

impl ServiceHandler for ViDisplayService {
    fn service_name(&self) -> &str {
        "vi:IApplicationDisplayService"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("vi:display: cmd_id={}", cmd_id);

        match cmd_id {
            // GetRelayService (100): returns move handle to IHOSBinderDriver
            100 => {
                log::info!("vi:display: GetRelayService");
                IpcResponse::success().with_move_handle(0)
            }

            // GetSystemDisplayService (101)
            101 => {
                log::info!("vi:display: GetSystemDisplayService");
                IpcResponse::success().with_move_handle(0)
            }

            // GetManagerDisplayService (102)
            102 => {
                log::info!("vi:display: GetManagerDisplayService");
                IpcResponse::success().with_move_handle(0)
            }

            // GetIndirectDisplayTransactionService (103)
            103 => {
                log::info!("vi:display: GetIndirectDisplayTransactionService");
                IpcResponse::success().with_move_handle(0)
            }

            // OpenDisplay (1010): return display_id
            1010 => {
                let display_id = self.next_display_id;
                self.next_display_id += 1;
                log::info!("vi:display: OpenDisplay -> id={}", display_id);
                IpcResponse::success_with_data(vec![display_id as u32, 0])
            }

            // CloseDisplay (1020)
            1020 => {
                log::info!("vi:display: CloseDisplay");
                IpcResponse::success()
            }

            // OpenLayer (2020): return NativeWindow parcel
            2020 => {
                let layer_id = self.next_layer_id;
                self.next_layer_id += 1;
                log::info!("vi:display: OpenLayer -> layer_id={}", layer_id);

                // Build NativeWindow parcel.
                let parcel_bytes = build_native_window_parcel(layer_id);
                let mut data = vec![parcel_bytes.len() as u32];
                // Pack parcel bytes into u32 words.
                for chunk in parcel_bytes.chunks(4) {
                    let mut word_bytes = [0u8; 4];
                    word_bytes[..chunk.len()].copy_from_slice(chunk);
                    data.push(u32::from_le_bytes(word_bytes));
                }
                IpcResponse::success_with_data(data)
            }

            // CreateStrayLayer (2030): return layer_id + NativeWindow parcel
            2030 => {
                let layer_id = self.next_layer_id;
                self.next_layer_id += 1;
                log::info!("vi:display: CreateStrayLayer -> layer_id={}", layer_id);

                // Build NativeWindow parcel.
                let parcel_bytes = build_native_window_parcel(layer_id);
                // Response: layer_id (u64 = 2 words) + parcel_size (u32) + parcel data
                let mut data = vec![layer_id as u32, 0]; // layer_id as u64
                data.push(parcel_bytes.len() as u32);    // parcel size
                for chunk in parcel_bytes.chunks(4) {
                    let mut word_bytes = [0u8; 4];
                    word_bytes[..chunk.len()].copy_from_slice(chunk);
                    data.push(u32::from_le_bytes(word_bytes));
                }
                IpcResponse::success_with_data(data)
            }

            // DestroyStrayLayer (2031)
            2031 => {
                log::info!("vi:display: DestroyStrayLayer");
                IpcResponse::success()
            }

            // SetLayerScalingMode (2101)
            2101 => {
                log::info!("vi:display: SetLayerScalingMode");
                IpcResponse::success()
            }

            // GetDisplayVsyncEvent (5202): return event handle
            5202 => {
                log::info!("vi:display: GetDisplayVsyncEvent");
                if let Some(handle) = self.vsync_event_handle {
                    IpcResponse::success().with_copy_handle(handle)
                } else {
                    IpcResponse::success()
                }
            }

            _ => {
                log::warn!("vi:display: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

/// Build a NativeWindow Parcel containing the binder info for a layer.
fn build_native_window_parcel(layer_id: u64) -> Vec<u8> {
    let mut parcel = Parcel::new();
    // Binder native window structure (simplified):
    // flat_binder_object: type(u32)=2, flags(u32)=0, binder_id(u64)=layer_id, cookie(u64)=0
    parcel.write_u32(2);              // type: BINDER_TYPE_HANDLE
    parcel.write_u32(0);              // flags
    parcel.write_u64(layer_id);       // binder id
    parcel.write_u64(0);              // cookie
    parcel.serialize()
}

// ── IHOSBinderDriver (ViBinderService) ──────────────────────────────────────

/// Binder driver sub-interface for buffer queue operations.
///
/// Uses the GPU context's NvMap registry to resolve nvmap handles to guest
/// addresses when setting preallocated buffers.
pub struct ViBinderService {
    buffer_queue: Arc<RwLock<BufferQueue>>,
    gpu: Arc<GpuContext>,
}

impl ViBinderService {
    pub fn new(buffer_queue: Arc<RwLock<BufferQueue>>, gpu: Arc<GpuContext>) -> Self {
        Self { buffer_queue, gpu }
    }
}

/// Binder transact codes (from Android IGraphicBufferProducer).
mod transact {
    pub const REQUEST_BUFFER: u32 = 1;
    pub const DEQUEUE_BUFFER: u32 = 2;
    pub const QUEUE_BUFFER: u32 = 4;
    pub const CANCEL_BUFFER: u32 = 5;
    pub const QUERY: u32 = 6;
    pub const CONNECT: u32 = 7;
    pub const DISCONNECT: u32 = 8;
    pub const SET_PREALLOCATED_BUFFER: u32 = 14;
}

impl ServiceHandler for ViBinderService {
    fn service_name(&self) -> &str {
        "vi:IHOSBinderDriver"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("vi:binder: cmd_id={}", cmd_id);

        match cmd_id {
            // TransactParcel (0): binder_id(u32), transact_code(u32), flags(u32), parcel data
            0 => {
                let binder_id = command.raw_data.first().copied().unwrap_or(0);
                let transact_code = command.raw_data.get(1).copied().unwrap_or(0);
                let _flags = command.raw_data.get(2).copied().unwrap_or(0);

                // Reconstruct input parcel from remaining raw_data.
                let parcel_words = if command.raw_data.len() > 3 {
                    &command.raw_data[3..]
                } else {
                    &[]
                };
                let parcel_bytes: Vec<u8> = parcel_words
                    .iter()
                    .flat_map(|w| w.to_le_bytes())
                    .collect();

                log::debug!(
                    "vi:binder: TransactParcel binder={}, code={}, data_len={}",
                    binder_id,
                    transact_code,
                    parcel_bytes.len()
                );

                let response_parcel =
                    self.handle_transact(transact_code, &parcel_bytes);

                // Pack response parcel into u32 words.
                let serialized = response_parcel.serialize();
                let mut data = Vec::new();
                for chunk in serialized.chunks(4) {
                    let mut word_bytes = [0u8; 4];
                    word_bytes[..chunk.len()].copy_from_slice(chunk);
                    data.push(u32::from_le_bytes(word_bytes));
                }

                IpcResponse::success_with_data(data)
            }

            // AdjustRefcount (1)
            1 => {
                log::debug!("vi:binder: AdjustRefcount");
                IpcResponse::success()
            }

            // GetNativeHandle (2)
            2 => {
                log::debug!("vi:binder: GetNativeHandle");
                IpcResponse::success()
            }

            _ => {
                log::warn!("vi:binder: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

impl ViBinderService {
    fn handle_transact(&mut self, code: u32, parcel_bytes: &[u8]) -> Parcel {
        let mut _input = Parcel::from_bytes(parcel_bytes);
        let mut response = Parcel::new();

        match code {
            transact::REQUEST_BUFFER => {
                let slot = _input.read_i32() as usize;
                log::debug!("vi:binder: RequestBuffer slot={}", slot);

                let bq = self.buffer_queue.read();
                if let Some(buf) = bq.get_buffer(slot) {
                    // Write GraphicBuffer to response.
                    response.write_i32(1); // has_buffer = true
                    response.write_u32(buf.width);
                    response.write_u32(buf.height);
                    response.write_u32(buf.stride);
                    response.write_u32(buf.format);
                    response.write_u32(0); // usage
                    response.write_u32(buf.nvmap_handle);
                    response.write_u32(0); // status = OK
                } else {
                    response.write_i32(0); // has_buffer = false
                    response.write_u32(0); // status = error
                }
            }

            transact::DEQUEUE_BUFFER => {
                log::debug!("vi:binder: DequeueBuffer");
                let mut bq = self.buffer_queue.write();
                match bq.dequeue() {
                    Some(slot) => {
                        response.write_i32(slot as i32); // slot
                        response.write_i32(0);           // fence (none)
                        response.write_u32(0);           // status = OK
                    }
                    None => {
                        response.write_i32(-1); // no slot
                        response.write_i32(0);
                        response.write_u32(0);  // still "OK" to avoid game crash
                    }
                }
            }

            transact::QUEUE_BUFFER => {
                let slot = _input.read_i32() as usize;
                log::debug!("vi:binder: QueueBuffer slot={}", slot);
                let mut bq = self.buffer_queue.write();
                bq.queue(slot);
                // Return QueueBufferOutput.
                response.write_u32(1280); // width
                response.write_u32(720);  // height
                response.write_u32(0);    // transform_hint
                response.write_u32(1);    // num_pending_buffers
                response.write_u32(0);    // status = OK
            }

            transact::CANCEL_BUFFER => {
                log::debug!("vi:binder: CancelBuffer");
                response.write_u32(0); // status = OK
            }

            transact::QUERY => {
                let what = _input.read_i32();
                log::debug!("vi:binder: Query what={}", what);
                // Common queries: 1=width, 2=height, 3=format
                let value = match what {
                    1 => 1280,           // DEFAULT_WIDTH
                    2 => 720,            // DEFAULT_HEIGHT
                    3 => 1,              // PIXEL_FORMAT_RGBA_8888
                    6 => 0,              // CONSUMER_USAGE
                    _ => 0,
                };
                response.write_i32(value);
                response.write_u32(0); // status = OK
            }

            transact::CONNECT => {
                log::debug!("vi:binder: Connect");
                // Return QueueBufferOutput.
                response.write_u32(1280); // width
                response.write_u32(720);  // height
                response.write_u32(0);    // transform_hint
                response.write_u32(1);    // num_pending_buffers
                response.write_u32(0);    // status = OK
            }

            transact::DISCONNECT => {
                log::debug!("vi:binder: Disconnect");
                response.write_u32(0); // status = OK
            }

            transact::SET_PREALLOCATED_BUFFER => {
                let slot = _input.read_i32() as usize;
                log::debug!("vi:binder: SetPreallocatedBuffer slot={}", slot);

                // Read GraphicBuffer from the parcel.
                let _has_buffer = _input.read_i32();
                let width = _input.read_u32();
                let height = _input.read_u32();
                let stride = _input.read_u32();
                let format = _input.read_u32();
                let _usage = _input.read_u32();
                let nvmap_handle = _input.read_u32();

                // Resolve the nvmap handle to a guest address via the registry.
                let offset = self
                    .gpu
                    .nvmap_registry
                    .get_address(nvmap_handle)
                    .unwrap_or(0);

                let buf = GraphicBuffer {
                    width,
                    height,
                    format,
                    nvmap_handle,
                    offset,
                    stride,
                };

                let mut bq = self.buffer_queue.write();
                bq.set_buffer(slot, buf);

                response.write_u32(0); // status = OK
            }

            _ => {
                log::warn!("vi:binder: unknown transact code={}", code);
                response.write_u32(0); // status = OK
            }
        }

        response
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn test_gpu() -> Arc<GpuContext> {
        Arc::new(GpuContext::new())
    }

    fn make_command(cmd_id: u32) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data: Vec::new(),
            b_buf_addrs: Vec::new(),
        }
    }

    fn make_command_with_data(cmd_id: u32, raw_data: Vec<u32>) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data,
            b_buf_addrs: Vec::new(),
        }
    }

    #[test]
    fn test_vi_manager_get_display_service() {
        let mut svc = ViManagerService::new();
        let cmd = make_command(2);
        let resp = svc.handle_request(2, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move.len(), 1);
    }

    #[test]
    fn test_vi_display_open_display() {
        let bq = Arc::new(RwLock::new(BufferQueue::new()));
        let mut svc = ViDisplayService::new(bq);
        let cmd = make_command(1010);
        let resp = svc.handle_request(1010, &cmd);
        assert!(resp.result.is_success());
        assert!(!resp.data.is_empty());
    }

    #[test]
    fn test_vi_display_create_stray_layer() {
        let bq = Arc::new(RwLock::new(BufferQueue::new()));
        let mut svc = ViDisplayService::new(bq);
        let cmd = make_command(2030);
        let resp = svc.handle_request(2030, &cmd);
        assert!(resp.result.is_success());
        assert!(resp.data.len() >= 3); // layer_id + parcel_size + data
    }

    #[test]
    fn test_vi_display_vsync_event() {
        let bq = Arc::new(RwLock::new(BufferQueue::new()));
        let mut svc = ViDisplayService::new(bq);
        svc.set_vsync_event_handle(42);

        let cmd = make_command(5202);
        let resp = svc.handle_request(5202, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_copy, vec![42]);
    }

    #[test]
    fn test_vi_binder_connect() {
        let bq = Arc::new(RwLock::new(BufferQueue::new()));
        let mut svc = ViBinderService::new(bq, test_gpu());
        let cmd = make_command_with_data(
            0,
            vec![0, transact::CONNECT, 0], // binder_id=0, code=CONNECT, flags=0
        );
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_vi_binder_dequeue_queue_cycle() {
        let bq = Arc::new(RwLock::new(BufferQueue::new()));
        {
            let mut q = bq.write();
            q.set_buffer(
                0,
                GraphicBuffer {
                    width: 1280,
                    height: 720,
                    format: 1,
                    nvmap_handle: 1,
                    offset: 0,
                    stride: 1280,
                },
            );
        }

        let mut svc = ViBinderService::new(bq.clone(), test_gpu());

        // Dequeue
        let cmd = make_command_with_data(
            0,
            vec![0, transact::DEQUEUE_BUFFER, 0],
        );
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());

        // Queue (slot 0)
        let mut queue_parcel = Parcel::new();
        queue_parcel.write_i32(0); // slot = 0
        let queue_bytes = queue_parcel.serialize();
        let mut queue_words: Vec<u32> = vec![0, transact::QUEUE_BUFFER, 0];
        for chunk in queue_bytes.chunks(4) {
            let mut wb = [0u8; 4];
            wb[..chunk.len()].copy_from_slice(chunk);
            queue_words.push(u32::from_le_bytes(wb));
        }
        let cmd = make_command_with_data(0, queue_words);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());

        // Verify buffer is queued.
        let q = bq.read();
        assert!(q.get_buffer(0).is_some());
    }

    #[test]
    fn test_set_preallocated_resolves_offset() {
        let gpu = test_gpu();
        // Pre-register nvmap handle 5 with address 0xCAFE_0000.
        gpu.nvmap_registry.register(5, 0xCAFE_0000, 0x1000);

        let bq = Arc::new(RwLock::new(BufferQueue::new()));
        let mut svc = ViBinderService::new(bq.clone(), gpu);

        // Build SET_PREALLOCATED_BUFFER parcel:
        // slot=0, has_buffer=1, width=1280, height=720, stride=1280,
        // format=1, usage=0, nvmap_handle=5
        let mut parcel = Parcel::new();
        parcel.write_i32(0);    // slot
        parcel.write_i32(1);    // has_buffer
        parcel.write_u32(1280); // width
        parcel.write_u32(720);  // height
        parcel.write_u32(1280); // stride
        parcel.write_u32(1);    // format (RGBA8888)
        parcel.write_u32(0);    // usage
        parcel.write_u32(5);    // nvmap_handle
        let parcel_bytes = parcel.serialize();
        let mut words: Vec<u32> = vec![0, transact::SET_PREALLOCATED_BUFFER, 0];
        for chunk in parcel_bytes.chunks(4) {
            let mut wb = [0u8; 4];
            wb[..chunk.len()].copy_from_slice(chunk);
            words.push(u32::from_le_bytes(wb));
        }

        let cmd = make_command_with_data(0, words);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());

        // Verify the buffer has the resolved offset.
        let q = bq.read();
        let buf = q.get_buffer(0).expect("buffer should be set");
        assert_eq!(buf.offset, 0xCAFE_0000);
        assert_eq!(buf.nvmap_handle, 5);
    }
}

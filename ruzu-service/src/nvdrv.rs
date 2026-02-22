// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `nvdrv` / `nvdrv:a` / `nvdrv:s` / `nvdrv:t` -- NVIDIA driver service.
//!
//! Routes `Open`, `Ioctl`, and `Close` commands to the appropriate device
//! implementations in `nvdrv_devices`.

use std::collections::HashMap;

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use crate::nvdrv_devices::{self, NvDevice};

/// HLE implementation of the NVIDIA driver service family.
pub struct NvdrvService {
    /// Map of file descriptor → device instance.
    devices: HashMap<u32, Box<dyn NvDevice>>,
    /// Next file descriptor to assign.
    next_fd: u32,
}

impl NvdrvService {
    pub fn new() -> Self {
        Self {
            devices: HashMap::new(),
            next_fd: 1,
        }
    }

    /// Read a null-terminated device path from raw_data words.
    fn read_device_path(raw_data: &[u32]) -> String {
        let mut bytes = Vec::new();
        for &word in raw_data {
            let wb = word.to_le_bytes();
            for &b in &wb {
                if b == 0 {
                    return String::from_utf8_lossy(&bytes).into_owned();
                }
                bytes.push(b);
            }
        }
        String::from_utf8_lossy(&bytes).into_owned()
    }
}

impl Default for NvdrvService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for NvdrvService {
    fn service_name(&self) -> &str {
        "nvdrv"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("nvdrv: cmd_id={}", cmd_id);

        match cmd_id {
            // ── Open (0) ─────────────────────────────────────────────────
            // raw_data contains the null-terminated device path.
            // Returns: [fd, error_code]
            0 => {
                let path = Self::read_device_path(&command.raw_data);
                log::info!("nvdrv: Open(\"{}\")", path);

                match nvdrv_devices::create_device(&path) {
                    Some(device) => {
                        let fd = self.next_fd;
                        self.next_fd += 1;
                        self.devices.insert(fd, device);
                        log::debug!("nvdrv: Open -> fd={}", fd);
                        IpcResponse::success_with_data(vec![fd, 0])
                    }
                    None => {
                        log::warn!("nvdrv: Open unknown device \"{}\"", path);
                        // Return fd=0 (invalid) with error code 0 to avoid crashes.
                        IpcResponse::success_with_data(vec![0, 0])
                    }
                }
            }

            // ── Ioctl (1) ────────────────────────────────────────────────
            // raw_data[0] = fd, raw_data[1] = ioctl_cmd, rest = ioctl input
            // Returns: [error_code]
            1 => {
                let fd = command.raw_data.first().copied().unwrap_or(0);
                let ioctl_cmd = command.raw_data.get(1).copied().unwrap_or(0);

                // Build input buffer from remaining raw_data words.
                let input_words = if command.raw_data.len() > 2 {
                    &command.raw_data[2..]
                } else {
                    &[]
                };
                let input: Vec<u8> = input_words
                    .iter()
                    .flat_map(|w| w.to_le_bytes())
                    .collect();

                let mut output = vec![0u8; 256];

                let nv_result = if let Some(device) = self.devices.get_mut(&fd) {
                    log::debug!(
                        "nvdrv: Ioctl fd={} ({}), cmd=0x{:X}",
                        fd,
                        device.name(),
                        ioctl_cmd
                    );
                    device.ioctl(ioctl_cmd, &input, &mut output)
                } else {
                    log::warn!("nvdrv: Ioctl on invalid fd={}", fd);
                    0
                };

                // Convert output bytes back to u32 words for the IPC response.
                let mut data = vec![nv_result];
                for chunk in output.chunks(4) {
                    if chunk.len() == 4 {
                        let word = u32::from_le_bytes(chunk.try_into().unwrap());
                        data.push(word);
                        // Limit response data size.
                        if data.len() >= 32 {
                            break;
                        }
                    }
                }

                IpcResponse::success_with_data(data)
            }

            // ── Close (2) ────────────────────────────────────────────────
            2 => {
                let fd = command.raw_data.first().copied().unwrap_or(0);
                log::info!("nvdrv: Close fd={}", fd);
                self.devices.remove(&fd);
                IpcResponse::success_with_data(vec![0])
            }

            // ── Initialize (3) ───────────────────────────────────────────
            3 => {
                log::info!("nvdrv: Initialize");
                IpcResponse::success()
            }

            // ── QueryEvent (4) ───────────────────────────────────────────
            4 => {
                log::info!("nvdrv: QueryEvent");
                IpcResponse::success()
            }

            // ── SetAruid (8) ─────────────────────────────────────────────
            8 => {
                log::info!("nvdrv: SetAruid");
                IpcResponse::success_with_data(vec![0])
            }

            // ── Everything else ──────────────────────────────────────────
            _ => {
                log::warn!("nvdrv: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn make_command(cmd_id: u32, raw_data: Vec<u32>) -> IpcCommand {
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
        }
    }

    /// Encode a device path as raw_data words.
    fn path_to_words(path: &str) -> Vec<u32> {
        let mut bytes = path.as_bytes().to_vec();
        bytes.push(0); // null terminator
        while bytes.len() % 4 != 0 {
            bytes.push(0);
        }
        bytes
            .chunks(4)
            .map(|c| u32::from_le_bytes(c.try_into().unwrap()))
            .collect()
    }

    #[test]
    fn test_open_nvmap() {
        let mut svc = NvdrvService::new();
        let cmd = make_command(0, path_to_words("/dev/nvmap"));
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert!(resp.data.len() >= 2);
        assert_ne!(resp.data[0], 0); // fd should be non-zero
        assert_eq!(resp.data[1], 0); // error code
    }

    #[test]
    fn test_open_unknown_device() {
        let mut svc = NvdrvService::new();
        let cmd = make_command(0, path_to_words("/dev/unknown"));
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data[0], 0); // fd = 0 (invalid)
    }

    #[test]
    fn test_ioctl_on_opened_device() {
        let mut svc = NvdrvService::new();

        // Open nvmap
        let open_cmd = make_command(0, path_to_words("/dev/nvmap"));
        let open_resp = svc.handle_request(0, &open_cmd);
        let fd = open_resp.data[0];

        // Ioctl: Create (1), size = 0x1000
        let ioctl_cmd = make_command(1, vec![fd, 1, 0x1000]);
        let ioctl_resp = svc.handle_request(1, &ioctl_cmd);
        assert!(ioctl_resp.result.is_success());
        assert_eq!(ioctl_resp.data[0], 0); // nv_result = success
    }

    #[test]
    fn test_close() {
        let mut svc = NvdrvService::new();

        // Open
        let open_cmd = make_command(0, path_to_words("/dev/nvmap"));
        let open_resp = svc.handle_request(0, &open_cmd);
        let fd = open_resp.data[0];

        // Close
        let close_cmd = make_command(2, vec![fd]);
        let close_resp = svc.handle_request(2, &close_cmd);
        assert!(close_resp.result.is_success());

        // Device should be removed.
        assert!(!svc.devices.contains_key(&fd));
    }
}

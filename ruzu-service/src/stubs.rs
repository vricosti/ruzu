// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Stub service implementations.
//!
//! Each service accepts all commands and returns success. Games query these
//! services during startup; providing stubs avoids "unknown service" panics.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

macro_rules! stub_service {
    ($name:ident, $service_name:expr) => {
        pub struct $name;

        impl $name {
            pub fn new() -> Self {
                Self
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self::new()
            }
        }

        impl ServiceHandler for $name {
            fn service_name(&self) -> &str {
                $service_name
            }

            fn handle_request(&mut self, cmd_id: u32, _cmd: &IpcCommand) -> IpcResponse {
                log::debug!("{}: stub cmd_id={}", $service_name, cmd_id);
                IpcResponse::success()
            }
        }
    };
}

stub_service!(FatalService, "fatal:u");
stub_service!(PrepoService, "prepo:a");
stub_service!(CapsService, "caps:su");
stub_service!(NsService, "ns:am2");
stub_service!(BcatService, "bcat:a");
stub_service!(AocService, "aoc:u");
stub_service!(SplService, "spl:");
stub_service!(MiiService, "mii:e");
stub_service!(ErptService, "erpt:c");
stub_service!(EupldService, "eupld:c");
stub_service!(PmService, "pm:shell");
stub_service!(LdrService, "ldr:ro");
stub_service!(GlueService, "glue:u");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::{CommandType, IpcCommand};

    fn make_cmd(cmd_id: u32) -> IpcCommand {
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

    macro_rules! test_stub {
        ($test_name:ident, $service:ident) => {
            #[test]
            fn $test_name() {
                let mut svc = $service::new();
                let cmd = make_cmd(0);
                let resp = svc.handle_request(0, &cmd);
                assert!(resp.result.is_success());
            }
        };
    }

    test_stub!(test_fatal_stub, FatalService);
    test_stub!(test_prepo_stub, PrepoService);
    test_stub!(test_caps_stub, CapsService);
    test_stub!(test_ns_stub, NsService);
    test_stub!(test_bcat_stub, BcatService);
    test_stub!(test_aoc_stub, AocService);
    test_stub!(test_spl_stub, SplService);
    test_stub!(test_mii_stub, MiiService);
    test_stub!(test_erpt_stub, ErptService);
    test_stub!(test_eupld_stub, EupldService);
    test_stub!(test_pm_stub, PmService);
    test_stub!(test_ldr_stub, LdrService);
    test_stub!(test_glue_stub, GlueService);
}

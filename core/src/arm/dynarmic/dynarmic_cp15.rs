// SPDX-FileCopyrightText: 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/dynarmic_cp15.h and dynarmic_cp15.cpp
//! CP15 coprocessor emulation for ARM32 mode.

use std::sync::atomic::{fence, Ordering};

/// CP15 coprocessor register identifiers.
///
/// Corresponds to upstream `Dynarmic::A32::CoprocReg`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CoprocReg {
    C0 = 0,
    C1 = 1,
    C2 = 2,
    C3 = 3,
    C4 = 4,
    C5 = 5,
    C6 = 6,
    C7 = 7,
    C8 = 8,
    C9 = 9,
    C10 = 10,
    C11 = 11,
    C12 = 12,
    C13 = 13,
    C14 = 14,
    C15 = 15,
}

/// CP15 coprocessor state.
///
/// Corresponds to upstream `Core::DynarmicCP15`.
/// In the C++ version, this implements `Dynarmic::A32::Coprocessor`.
/// Here we store the state and provide the compile methods as regular functions
/// until rdynarmic is integrated.
pub struct DynarmicCP15 {
    /// User read/write thread register (CP15 c13 c0 2)
    pub uprw: u32,
    /// User read-only thread register (CP15 c13 c0 3)
    pub uro: u32,
    /// Dummy value for flush prefetch buffer writes
    dummy_value: u32,
}

impl DynarmicCP15 {
    pub fn new() -> Self {
        Self {
            uprw: 0,
            uro: 0,
            dummy_value: 0,
        }
    }

    /// Handle MCR (write to coprocessor) operations.
    ///
    /// Corresponds to upstream `DynarmicCP15::CompileSendOneWord`.
    pub fn compile_send_one_word(
        &mut self,
        two: bool,
        opc1: u32,
        crn: CoprocReg,
        crm: CoprocReg,
        opc2: u32,
    ) -> SendOneWordResult {
        if !two && crn == CoprocReg::C7 && opc1 == 0 && crm == CoprocReg::C5 && opc2 == 4 {
            // CP15_FLUSH_PREFETCH_BUFFER - dummy write, ignored
            return SendOneWordResult::DummyWrite;
        }

        if !two && crn == CoprocReg::C7 && opc1 == 0 && crm == CoprocReg::C10 {
            match opc2 {
                4 => {
                    // CP15_DATA_SYNC_BARRIER
                    fence(Ordering::SeqCst);
                    return SendOneWordResult::DataSyncBarrier;
                }
                5 => {
                    // CP15_DATA_MEMORY_BARRIER
                    fence(Ordering::SeqCst);
                    return SendOneWordResult::DataMemoryBarrier;
                }
                _ => {}
            }
        }

        if !two && crn == CoprocReg::C13 && opc1 == 0 && crm == CoprocReg::C0 && opc2 == 2 {
            // CP15_THREAD_UPRW
            return SendOneWordResult::ThreadUprw;
        }

        log::error!(
            "CP15: mcr{} p15, {}, <Rt>, {:?}, {:?}, {}",
            if two { "2" } else { "" },
            opc1,
            crn,
            crm,
            opc2
        );
        SendOneWordResult::Unhandled
    }

    /// Handle MRC (read from coprocessor) operations.
    ///
    /// Corresponds to upstream `DynarmicCP15::CompileGetOneWord`.
    pub fn compile_get_one_word(
        &self,
        two: bool,
        opc1: u32,
        crn: CoprocReg,
        crm: CoprocReg,
        opc2: u32,
    ) -> GetOneWordResult {
        if !two && crn == CoprocReg::C13 && opc1 == 0 && crm == CoprocReg::C0 {
            match opc2 {
                2 => return GetOneWordResult::ThreadUprw(self.uprw),
                3 => return GetOneWordResult::ThreadUro(self.uro),
                _ => {}
            }
        }

        log::error!(
            "CP15: mrc{} p15, {}, <Rt>, {:?}, {:?}, {}",
            if two { "2" } else { "" },
            opc1,
            crn,
            crm,
            opc2
        );
        GetOneWordResult::Unhandled
    }

    /// Handle MRRC (read two words from coprocessor) operations.
    ///
    /// Corresponds to upstream `DynarmicCP15::CompileGetTwoWords`.
    pub fn compile_get_two_words(&self, two: bool, opc: u32, crm: CoprocReg) -> GetTwoWordsResult {
        if !two && opc == 0 && crm == CoprocReg::C14 {
            // CNTPCT - return clock ticks
            return GetTwoWordsResult::Cntpct;
        }

        log::error!(
            "CP15: mrrc{} p15, {}, <Rt>, <Rt2>, {:?}",
            if two { "2" } else { "" },
            opc,
            crm
        );
        GetTwoWordsResult::Unhandled
    }

    /// Handle CDP (coprocessor data processing) operations.
    ///
    /// Corresponds to upstream `DynarmicCP15::CompileInternalOperation`.
    pub fn compile_internal_operation(
        &self,
        two: bool,
        opc1: u32,
        crd: CoprocReg,
        crn: CoprocReg,
        crm: CoprocReg,
        opc2: u32,
    ) {
        log::error!(
            "CP15: cdp{} p15, {}, {:?}, {:?}, {:?}, {}",
            if two { "2" } else { "" },
            opc1,
            crd,
            crn,
            crm,
            opc2
        );
    }

    /// Handle MCRR (write two words) operations.
    ///
    /// Corresponds to upstream `DynarmicCP15::CompileSendTwoWords`.
    pub fn compile_send_two_words(&self, two: bool, opc: u32, crm: CoprocReg) {
        log::error!(
            "CP15: mcrr{} p15, {}, <Rt>, <Rt2>, {:?}",
            if two { "2" } else { "" },
            opc,
            crm
        );
    }

    /// Handle LDC/STC (load/store coprocessor) operations.
    ///
    /// Corresponds to upstream `DynarmicCP15::CompileLoadWords` / `CompileStoreWords`.
    pub fn compile_load_words(
        &self,
        two: bool,
        long_transfer: bool,
        crd: CoprocReg,
        option: Option<u8>,
    ) {
        if let Some(opt) = option {
            log::error!(
                "CP15: mrrc{}{} p15, {:?}, [...], {}",
                if two { "2" } else { "" },
                if long_transfer { "l" } else { "" },
                crd,
                opt
            );
        } else {
            log::error!(
                "CP15: mrrc{}{} p15, {:?}, [...]",
                if two { "2" } else { "" },
                if long_transfer { "l" } else { "" },
                crd
            );
        }
    }

    pub fn compile_store_words(
        &self,
        two: bool,
        long_transfer: bool,
        crd: CoprocReg,
        option: Option<u8>,
    ) {
        if let Some(opt) = option {
            log::error!(
                "CP15: mrrc{}{} p15, {:?}, [...], {}",
                if two { "2" } else { "" },
                if long_transfer { "l" } else { "" },
                crd,
                opt
            );
        } else {
            log::error!(
                "CP15: mrrc{}{} p15, {:?}, [...]",
                if two { "2" } else { "" },
                if long_transfer { "l" } else { "" },
                crd
            );
        }
    }
}

/// Result from CompileSendOneWord
pub enum SendOneWordResult {
    DummyWrite,
    DataSyncBarrier,
    DataMemoryBarrier,
    ThreadUprw,
    Unhandled,
}

/// Result from CompileGetOneWord
pub enum GetOneWordResult {
    ThreadUprw(u32),
    ThreadUro(u32),
    Unhandled,
}

/// Result from CompileGetTwoWords
pub enum GetTwoWordsResult {
    Cntpct,
    Unhandled,
}

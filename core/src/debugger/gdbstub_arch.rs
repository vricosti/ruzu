// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/debugger/gdbstub_arch.h and gdbstub_arch.cpp
//! GDB stub architecture-specific register access for AArch64 and AArch32.

use crate::hle::kernel::k_thread::ThreadContext;

// --- Hex conversion helpers matching upstream HexToValue / ValueToHex ---

fn value_to_hex_u32(val: u32) -> String {
    hex::encode(val.to_le_bytes())
}

fn value_to_hex_u64(val: u64) -> String {
    hex::encode(val.to_le_bytes())
}

fn value_to_hex_u128(val: u128) -> String {
    hex::encode(val.to_le_bytes())
}

fn hex_to_u32(hex_str: &str) -> u32 {
    let bytes = hex::decode(hex_str).unwrap_or_default();
    let mut arr = [0u8; 4];
    let len = bytes.len().min(4);
    arr[..len].copy_from_slice(&bytes[..len]);
    u32::from_le_bytes(arr)
}

fn hex_to_u64(hex_str: &str) -> u64 {
    let bytes = hex::decode(hex_str).unwrap_or_default();
    let mut arr = [0u8; 8];
    let len = bytes.len().min(8);
    arr[..len].copy_from_slice(&bytes[..len]);
    u64::from_le_bytes(arr)
}

fn hex_to_u128(hex_str: &str) -> u128 {
    let bytes = hex::decode(hex_str).unwrap_or_default();
    let mut arr = [0u8; 16];
    let len = bytes.len().min(16);
    arr[..len].copy_from_slice(&bytes[..len]);
    u128::from_le_bytes(arr)
}

/// Abstract GDB stub architecture interface.
///
/// Corresponds to upstream `Core::GDBStubArch`.
pub trait GdbStubArch {
    fn get_target_xml(&self) -> &'static str;
    fn reg_read(&self, ctx: &ThreadContext, id: usize) -> String;
    fn reg_write(&self, ctx: &mut ThreadContext, id: usize, value: &str);
    fn read_registers(&self, ctx: &ThreadContext) -> String;
    fn write_registers(&self, ctx: &mut ThreadContext, register_data: &str);
    fn thread_status(&self, ctx: &ThreadContext, thread_id: u64, signal: u8) -> String;
    fn breakpoint_instruction(&self) -> u32;
}

/// AArch64 GDB stub architecture.
///
/// Corresponds to upstream `Core::GDBStubA64`.
pub struct GdbStubA64;

impl GdbStubA64 {
    // Register index constants matching upstream
    const FP_REGISTER: usize = 29;
    const LR_REGISTER: usize = 30;
    const SP_REGISTER: usize = 31;
    const PC_REGISTER: usize = 32;
    const PSTATE_REGISTER: usize = 33;
    const Q0_REGISTER: usize = 34;
    const FPSR_REGISTER: usize = 66;
    const FPCR_REGISTER: usize = 67;
}

impl GdbStubArch for GdbStubA64 {
    fn get_target_xml(&self) -> &'static str {
        // Matches upstream GDBStubA64::GetTargetXML()
        r#"<?xml version="1.0"?>
<!DOCTYPE target SYSTEM "gdb-target.dtd">
<target version="1.0">
  <architecture>aarch64</architecture>
  <feature name="org.gnu.gdb.aarch64.core">
    <reg name="x0" bitsize="64"/>
    <reg name="x1" bitsize="64"/>
    <reg name="x2" bitsize="64"/>
    <reg name="x3" bitsize="64"/>
    <reg name="x4" bitsize="64"/>
    <reg name="x5" bitsize="64"/>
    <reg name="x6" bitsize="64"/>
    <reg name="x7" bitsize="64"/>
    <reg name="x8" bitsize="64"/>
    <reg name="x9" bitsize="64"/>
    <reg name="x10" bitsize="64"/>
    <reg name="x11" bitsize="64"/>
    <reg name="x12" bitsize="64"/>
    <reg name="x13" bitsize="64"/>
    <reg name="x14" bitsize="64"/>
    <reg name="x15" bitsize="64"/>
    <reg name="x16" bitsize="64"/>
    <reg name="x17" bitsize="64"/>
    <reg name="x18" bitsize="64"/>
    <reg name="x19" bitsize="64"/>
    <reg name="x20" bitsize="64"/>
    <reg name="x21" bitsize="64"/>
    <reg name="x22" bitsize="64"/>
    <reg name="x23" bitsize="64"/>
    <reg name="x24" bitsize="64"/>
    <reg name="x25" bitsize="64"/>
    <reg name="x26" bitsize="64"/>
    <reg name="x27" bitsize="64"/>
    <reg name="x28" bitsize="64"/>
    <reg name="x29" bitsize="64"/>
    <reg name="x30" bitsize="64"/>
    <reg name="sp" bitsize="64" type="data_ptr"/>
    <reg name="pc" bitsize="64" type="code_ptr"/>
    <flags id="cpsr_flags" size="4">
      <field name="SP" start="0" end="0"/>
      <field name="" start="1" end="1"/>
      <field name="EL" start="2" end="3"/>
      <field name="nRW" start="4" end="4"/>
      <field name="" start="5" end="5"/>
      <field name="F" start="6" end="6"/>
      <field name="I" start="7" end="7"/>
      <field name="A" start="8" end="8"/>
      <field name="D" start="9" end="9"/>
      <field name="IL" start="20" end="20"/>
      <field name="SS" start="21" end="21"/>
      <field name="V" start="28" end="28"/>
      <field name="C" start="29" end="29"/>
      <field name="Z" start="30" end="30"/>
      <field name="N" start="31" end="31"/>
    </flags>
    <reg name="cpsr" bitsize="32" type="cpsr_flags"/>
  </feature>
  <feature name="org.gnu.gdb.aarch64.fpu">
    <vector id="v2d" type="ieee_double" count="2"/>
    <vector id="v2u" type="uint64" count="2"/>
    <vector id="v2i" type="int64" count="2"/>
    <vector id="v4f" type="ieee_single" count="4"/>
    <vector id="v4u" type="uint32" count="4"/>
    <vector id="v4i" type="int32" count="4"/>
    <vector id="v8u" type="uint16" count="8"/>
    <vector id="v8i" type="int16" count="8"/>
    <vector id="v16u" type="uint8" count="16"/>
    <vector id="v16i" type="int8" count="16"/>
    <vector id="v1u" type="uint128" count="1"/>
    <vector id="v1i" type="int128" count="1"/>
    <union id="vnd">
      <field name="f" type="v2d"/>
      <field name="u" type="v2u"/>
      <field name="s" type="v2i"/>
    </union>
    <union id="vns">
      <field name="f" type="v4f"/>
      <field name="u" type="v4u"/>
      <field name="s" type="v4i"/>
    </union>
    <union id="vnh">
      <field name="u" type="v8u"/>
      <field name="s" type="v8i"/>
    </union>
    <union id="vnb">
      <field name="u" type="v16u"/>
      <field name="s" type="v16i"/>
    </union>
    <union id="vnq">
      <field name="u" type="v1u"/>
      <field name="s" type="v1i"/>
    </union>
    <union id="aarch64v">
      <field name="d" type="vnd"/>
      <field name="s" type="vns"/>
      <field name="h" type="vnh"/>
      <field name="b" type="vnb"/>
      <field name="q" type="vnq"/>
    </union>
    <reg name="v0" bitsize="128" type="aarch64v" regnum="34"/>
    <reg name="v1" bitsize="128" type="aarch64v" />
    <reg name="v2" bitsize="128" type="aarch64v" />
    <reg name="v3" bitsize="128" type="aarch64v" />
    <reg name="v4" bitsize="128" type="aarch64v" />
    <reg name="v5" bitsize="128" type="aarch64v" />
    <reg name="v6" bitsize="128" type="aarch64v" />
    <reg name="v7" bitsize="128" type="aarch64v" />
    <reg name="v8" bitsize="128" type="aarch64v" />
    <reg name="v9" bitsize="128" type="aarch64v" />
    <reg name="v10" bitsize="128" type="aarch64v"/>
    <reg name="v11" bitsize="128" type="aarch64v"/>
    <reg name="v12" bitsize="128" type="aarch64v"/>
    <reg name="v13" bitsize="128" type="aarch64v"/>
    <reg name="v14" bitsize="128" type="aarch64v"/>
    <reg name="v15" bitsize="128" type="aarch64v"/>
    <reg name="v16" bitsize="128" type="aarch64v"/>
    <reg name="v17" bitsize="128" type="aarch64v"/>
    <reg name="v18" bitsize="128" type="aarch64v"/>
    <reg name="v19" bitsize="128" type="aarch64v"/>
    <reg name="v20" bitsize="128" type="aarch64v"/>
    <reg name="v21" bitsize="128" type="aarch64v"/>
    <reg name="v22" bitsize="128" type="aarch64v"/>
    <reg name="v23" bitsize="128" type="aarch64v"/>
    <reg name="v24" bitsize="128" type="aarch64v"/>
    <reg name="v25" bitsize="128" type="aarch64v"/>
    <reg name="v26" bitsize="128" type="aarch64v"/>
    <reg name="v27" bitsize="128" type="aarch64v"/>
    <reg name="v28" bitsize="128" type="aarch64v"/>
    <reg name="v29" bitsize="128" type="aarch64v"/>
    <reg name="v30" bitsize="128" type="aarch64v"/>
    <reg name="v31" bitsize="128" type="aarch64v"/>
    <reg name="fpsr" bitsize="32"/>
    <reg name="fpcr" bitsize="32"/>
  </feature>
</target>"#
    }

    fn reg_read(&self, ctx: &ThreadContext, id: usize) -> String {
        if id < Self::FP_REGISTER {
            value_to_hex_u64(ctx.r[id])
        } else if id == Self::FP_REGISTER {
            value_to_hex_u64(ctx.fp)
        } else if id == Self::LR_REGISTER {
            value_to_hex_u64(ctx.lr)
        } else if id == Self::SP_REGISTER {
            value_to_hex_u64(ctx.sp)
        } else if id == Self::PC_REGISTER {
            value_to_hex_u64(ctx.pc)
        } else if id == Self::PSTATE_REGISTER {
            value_to_hex_u32(ctx.pstate)
        } else if id >= Self::Q0_REGISTER && id < Self::FPSR_REGISTER {
            value_to_hex_u128(ctx.v[id - Self::Q0_REGISTER])
        } else if id == Self::FPSR_REGISTER {
            value_to_hex_u32(ctx.fpsr)
        } else if id == Self::FPCR_REGISTER {
            value_to_hex_u32(ctx.fpcr)
        } else {
            String::new()
        }
    }

    fn reg_write(&self, ctx: &mut ThreadContext, id: usize, value: &str) {
        if id < Self::FP_REGISTER {
            ctx.r[id] = hex_to_u64(value);
        } else if id == Self::FP_REGISTER {
            ctx.fp = hex_to_u64(value);
        } else if id == Self::LR_REGISTER {
            ctx.lr = hex_to_u64(value);
        } else if id == Self::SP_REGISTER {
            ctx.sp = hex_to_u64(value);
        } else if id == Self::PC_REGISTER {
            ctx.pc = hex_to_u64(value);
        } else if id == Self::PSTATE_REGISTER {
            ctx.pstate = hex_to_u32(value);
        } else if id >= Self::Q0_REGISTER && id < Self::FPSR_REGISTER {
            ctx.v[id - Self::Q0_REGISTER] = hex_to_u128(value);
        } else if id == Self::FPSR_REGISTER {
            ctx.fpsr = hex_to_u32(value);
        } else if id == Self::FPCR_REGISTER {
            ctx.fpcr = hex_to_u32(value);
        }
    }

    fn read_registers(&self, ctx: &ThreadContext) -> String {
        let mut output = String::new();
        for reg in 0..=Self::FPCR_REGISTER {
            output += &self.reg_read(ctx, reg);
        }
        output
    }

    fn write_registers(&self, ctx: &mut ThreadContext, register_data: &str) {
        let mut i = 0;
        for reg in 0..=Self::FPCR_REGISTER {
            if reg <= Self::SP_REGISTER || reg == Self::PC_REGISTER {
                self.reg_write(ctx, reg, &register_data[i..i + 16]);
                i += 16;
            } else if reg == Self::PSTATE_REGISTER
                || reg == Self::FPCR_REGISTER
                || reg == Self::FPSR_REGISTER
            {
                self.reg_write(ctx, reg, &register_data[i..i + 8]);
                i += 8;
            } else if reg >= Self::Q0_REGISTER && reg < Self::FPCR_REGISTER {
                self.reg_write(ctx, reg, &register_data[i..i + 32]);
                i += 32;
            }
        }
    }

    fn thread_status(&self, ctx: &ThreadContext, thread_id: u64, signal: u8) -> String {
        format!(
            "T{:02x}{:02x}:{};{:02x}:{};{:02x}:{};thread:{:x};",
            signal,
            Self::PC_REGISTER,
            self.reg_read(ctx, Self::PC_REGISTER),
            Self::SP_REGISTER,
            self.reg_read(ctx, Self::SP_REGISTER),
            Self::LR_REGISTER,
            self.reg_read(ctx, Self::LR_REGISTER),
            thread_id
        )
    }

    fn breakpoint_instruction(&self) -> u32 {
        // A64: brk #0
        0xd4200000
    }
}

/// AArch32 GDB stub architecture.
///
/// Corresponds to upstream `Core::GDBStubA32`.
pub struct GdbStubA32;

impl GdbStubA32 {
    // Register index constants matching upstream
    const SP_REGISTER: usize = 13;
    const LR_REGISTER: usize = 14;
    const PC_REGISTER: usize = 15;
    const CPSR_REGISTER: usize = 25;
    const D0_REGISTER: usize = 32;
    const Q0_REGISTER: usize = 64;
    const FPSCR_REGISTER: usize = 80;
}

impl GdbStubArch for GdbStubA32 {
    fn get_target_xml(&self) -> &'static str {
        // Matches upstream GDBStubA32::GetTargetXML()
        r#"<?xml version="1.0"?>
<!DOCTYPE target SYSTEM "gdb-target.dtd">
<target version="1.0">
  <architecture>arm</architecture>
  <feature name="org.gnu.gdb.arm.core">
    <reg name="r0" bitsize="32" type="uint32"/>
    <reg name="r1" bitsize="32" type="uint32"/>
    <reg name="r2" bitsize="32" type="uint32"/>
    <reg name="r3" bitsize="32" type="uint32"/>
    <reg name="r4" bitsize="32" type="uint32"/>
    <reg name="r5" bitsize="32" type="uint32"/>
    <reg name="r6" bitsize="32" type="uint32"/>
    <reg name="r7" bitsize="32" type="uint32"/>
    <reg name="r8" bitsize="32" type="uint32"/>
    <reg name="r9" bitsize="32" type="uint32"/>
    <reg name="r10" bitsize="32" type="uint32"/>
    <reg name="r11" bitsize="32" type="uint32"/>
    <reg name="r12" bitsize="32" type="uint32"/>
    <reg name="sp" bitsize="32" type="data_ptr"/>
    <reg name="lr" bitsize="32" type="code_ptr"/>
    <reg name="pc" bitsize="32" type="code_ptr"/>
    <reg name="cpsr" bitsize="32" regnum="25"/>
  </feature>
  <feature name="org.gnu.gdb.arm.vfp">
    <vector id="neon_uint8x8" type="uint8" count="8"/>
    <vector id="neon_uint16x4" type="uint16" count="4"/>
    <vector id="neon_uint32x2" type="uint32" count="2"/>
    <vector id="neon_float32x2" type="ieee_single" count="2"/>
    <union id="neon_d">
      <field name="u8" type="neon_uint8x8"/>
      <field name="u16" type="neon_uint16x4"/>
      <field name="u32" type="neon_uint32x2"/>
      <field name="u64" type="uint64"/>
      <field name="f32" type="neon_float32x2"/>
      <field name="f64" type="ieee_double"/>
    </union>
    <vector id="neon_uint8x16" type="uint8" count="16"/>
    <vector id="neon_uint16x8" type="uint16" count="8"/>
    <vector id="neon_uint32x4" type="uint32" count="4"/>
    <vector id="neon_uint64x2" type="uint64" count="2"/>
    <vector id="neon_float32x4" type="ieee_single" count="4"/>
    <vector id="neon_float64x2" type="ieee_double" count="2"/>
    <union id="neon_q">
      <field name="u8" type="neon_uint8x16"/>
      <field name="u16" type="neon_uint16x8"/>
      <field name="u32" type="neon_uint32x4"/>
      <field name="u64" type="neon_uint64x2"/>
      <field name="f32" type="neon_float32x4"/>
      <field name="f64" type="neon_float64x2"/>
    </union>
    <reg name="d0" bitsize="64" type="neon_d" regnum="32"/>
    <reg name="d1" bitsize="64" type="neon_d"/>
    <reg name="d2" bitsize="64" type="neon_d"/>
    <reg name="d3" bitsize="64" type="neon_d"/>
    <reg name="d4" bitsize="64" type="neon_d"/>
    <reg name="d5" bitsize="64" type="neon_d"/>
    <reg name="d6" bitsize="64" type="neon_d"/>
    <reg name="d7" bitsize="64" type="neon_d"/>
    <reg name="d8" bitsize="64" type="neon_d"/>
    <reg name="d9" bitsize="64" type="neon_d"/>
    <reg name="d10" bitsize="64" type="neon_d"/>
    <reg name="d11" bitsize="64" type="neon_d"/>
    <reg name="d12" bitsize="64" type="neon_d"/>
    <reg name="d13" bitsize="64" type="neon_d"/>
    <reg name="d14" bitsize="64" type="neon_d"/>
    <reg name="d15" bitsize="64" type="neon_d"/>
    <reg name="d16" bitsize="64" type="neon_d"/>
    <reg name="d17" bitsize="64" type="neon_d"/>
    <reg name="d18" bitsize="64" type="neon_d"/>
    <reg name="d19" bitsize="64" type="neon_d"/>
    <reg name="d20" bitsize="64" type="neon_d"/>
    <reg name="d21" bitsize="64" type="neon_d"/>
    <reg name="d22" bitsize="64" type="neon_d"/>
    <reg name="d23" bitsize="64" type="neon_d"/>
    <reg name="d24" bitsize="64" type="neon_d"/>
    <reg name="d25" bitsize="64" type="neon_d"/>
    <reg name="d26" bitsize="64" type="neon_d"/>
    <reg name="d27" bitsize="64" type="neon_d"/>
    <reg name="d28" bitsize="64" type="neon_d"/>
    <reg name="d29" bitsize="64" type="neon_d"/>
    <reg name="d30" bitsize="64" type="neon_d"/>
    <reg name="d31" bitsize="64" type="neon_d"/>

    <reg name="q0" bitsize="128" type="neon_q" regnum="64"/>
    <reg name="q1" bitsize="128" type="neon_q"/>
    <reg name="q2" bitsize="128" type="neon_q"/>
    <reg name="q3" bitsize="128" type="neon_q"/>
    <reg name="q4" bitsize="128" type="neon_q"/>
    <reg name="q5" bitsize="128" type="neon_q"/>
    <reg name="q6" bitsize="128" type="neon_q"/>
    <reg name="q7" bitsize="128" type="neon_q"/>
    <reg name="q8" bitsize="128" type="neon_q"/>
    <reg name="q9" bitsize="128" type="neon_q"/>
    <reg name="q10" bitsize="128" type="neon_q"/>
    <reg name="q10" bitsize="128" type="neon_q"/>
    <reg name="q12" bitsize="128" type="neon_q"/>
    <reg name="q13" bitsize="128" type="neon_q"/>
    <reg name="q14" bitsize="128" type="neon_q"/>
    <reg name="q15" bitsize="128" type="neon_q"/>

    <reg name="fpscr" bitsize="32" type="int" group="float" regnum="80"/>
  </feature>
</target>"#
    }

    fn reg_read(&self, ctx: &ThreadContext, id: usize) -> String {
        if id <= Self::PC_REGISTER {
            value_to_hex_u32(ctx.r[id] as u32)
        } else if id == Self::CPSR_REGISTER {
            value_to_hex_u32(ctx.pstate)
        } else if id >= Self::D0_REGISTER && id < Self::Q0_REGISTER {
            // D registers are the lower/upper 64-bit halves of Q registers
            let q_idx = (id - Self::D0_REGISTER) / 2;
            let half = (id - Self::D0_REGISTER) % 2;
            let q_val = ctx.v[q_idx];
            let d_val = if half == 0 {
                q_val as u64
            } else {
                (q_val >> 64) as u64
            };
            value_to_hex_u64(d_val)
        } else if id >= Self::Q0_REGISTER && id < Self::FPSCR_REGISTER {
            value_to_hex_u128(ctx.v[id - Self::Q0_REGISTER])
        } else if id == Self::FPSCR_REGISTER {
            value_to_hex_u32(ctx.fpcr | ctx.fpsr)
        } else {
            String::new()
        }
    }

    fn reg_write(&self, ctx: &mut ThreadContext, id: usize, value: &str) {
        if id <= Self::PC_REGISTER {
            ctx.r[id] = hex_to_u32(value) as u64;
        } else if id == Self::CPSR_REGISTER {
            ctx.pstate = hex_to_u32(value);
        } else if id >= Self::D0_REGISTER && id < Self::Q0_REGISTER {
            let q_idx = (id - Self::D0_REGISTER) / 2;
            let half = (id - Self::D0_REGISTER) % 2;
            let d_val = hex_to_u64(value);
            if half == 0 {
                ctx.v[q_idx] = (ctx.v[q_idx] & !0xFFFF_FFFF_FFFF_FFFF) | d_val as u128;
            } else {
                ctx.v[q_idx] = (ctx.v[q_idx] & 0xFFFF_FFFF_FFFF_FFFF) | ((d_val as u128) << 64);
            }
        } else if id >= Self::Q0_REGISTER && id < Self::FPSCR_REGISTER {
            ctx.v[id - Self::Q0_REGISTER] = hex_to_u128(value);
        } else if id == Self::FPSCR_REGISTER {
            let val = hex_to_u32(value);
            ctx.fpcr = val;
            ctx.fpsr = val;
        }
    }

    fn read_registers(&self, ctx: &ThreadContext) -> String {
        let mut output = String::new();
        let mut reg = 0;
        while reg <= Self::FPSCR_REGISTER {
            let gpr = reg <= Self::PC_REGISTER;
            let dfpr = reg >= Self::D0_REGISTER && reg < Self::Q0_REGISTER;
            let qfpr = reg >= Self::Q0_REGISTER && reg < Self::FPSCR_REGISTER;

            if gpr || dfpr || qfpr || reg == Self::CPSR_REGISTER || reg == Self::FPSCR_REGISTER {
                output += &self.reg_read(ctx, reg);
            }
            reg += 1;
        }
        output
    }

    fn write_registers(&self, ctx: &mut ThreadContext, register_data: &str) {
        let mut i = 0;
        let mut reg = 0;
        while reg <= Self::FPSCR_REGISTER {
            let gpr = reg <= Self::PC_REGISTER;
            let dfpr = reg >= Self::D0_REGISTER && reg < Self::Q0_REGISTER;
            let qfpr = reg >= Self::Q0_REGISTER && reg < Self::FPSCR_REGISTER;

            if gpr || reg == Self::CPSR_REGISTER || reg == Self::FPSCR_REGISTER {
                if i + 8 <= register_data.len() {
                    self.reg_write(ctx, reg, &register_data[i..i + 8]);
                }
                i += 8;
            } else if dfpr {
                if i + 16 <= register_data.len() {
                    self.reg_write(ctx, reg, &register_data[i..i + 16]);
                }
                i += 16;
            } else if qfpr {
                if i + 32 <= register_data.len() {
                    self.reg_write(ctx, reg, &register_data[i..i + 32]);
                }
                i += 32;
            }

            // Upstream skips register gaps: after PC jumps to CPSR, after CPSR jumps to D0
            if reg == Self::PC_REGISTER {
                reg = Self::CPSR_REGISTER;
            } else if reg == Self::CPSR_REGISTER {
                reg = Self::D0_REGISTER;
            } else {
                reg += 1;
            }
        }
    }

    fn thread_status(&self, ctx: &ThreadContext, thread_id: u64, signal: u8) -> String {
        format!(
            "T{:02x}{:02x}:{};{:02x}:{};{:02x}:{};thread:{:x};",
            signal,
            Self::PC_REGISTER,
            self.reg_read(ctx, Self::PC_REGISTER),
            Self::SP_REGISTER,
            self.reg_read(ctx, Self::SP_REGISTER),
            Self::LR_REGISTER,
            self.reg_read(ctx, Self::LR_REGISTER),
            thread_id
        )
    }

    fn breakpoint_instruction(&self) -> u32 {
        // A32: trap + b #4 (T32 compatible)
        0xe7ffdefe
    }
}

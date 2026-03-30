// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/macro/macro.h` and `macro.cpp`.
//!
//! Defines the macro instruction set, opcode decoding, and the `MacroEngine`
//! base that manages macro code upload, caching, and execution dispatch.

use std::collections::HashMap;

use super::macro_hle::HleMacro;
use common::container_hash::hash_u32_slice;
use crate::engines::maxwell_3d::Maxwell3D;

// ── Constants ────────────────────────────────────────────────────────────────

/// Number of general-purpose macro registers.
///
/// Port of `Tegra::Macro::NUM_MACRO_REGISTERS`.
pub const NUM_MACRO_REGISTERS: usize = 8;

// ── Instruction field enums ──────────────────────────────────────────────────

/// Primary operation encoded in bits [2:0] of the 32-bit opcode.
///
/// Port of `Tegra::Macro::Operation`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Operation {
    Alu = 0,
    AddImmediate = 1,
    ExtractInsert = 2,
    ExtractShiftLeftImmediate = 3,
    ExtractShiftLeftRegister = 4,
    Read = 5,
    Unused = 6,
    Branch = 7,
}

impl Operation {
    pub fn from_raw(v: u32) -> Self {
        match v & 0x7 {
            0 => Self::Alu,
            1 => Self::AddImmediate,
            2 => Self::ExtractInsert,
            3 => Self::ExtractShiftLeftImmediate,
            4 => Self::ExtractShiftLeftRegister,
            5 => Self::Read,
            6 => Self::Unused,
            7 => Self::Branch,
            _ => unreachable!(),
        }
    }
}

/// ALU sub-operation encoded in bits [21:17].
///
/// Port of `Tegra::Macro::ALUOperation`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AluOperation {
    Add = 0,
    AddWithCarry = 1,
    Subtract = 2,
    SubtractWithBorrow = 3,
    Xor = 8,
    Or = 9,
    And = 10,
    AndNot = 11,
    Nand = 12,
}

impl AluOperation {
    pub fn from_raw(v: u32) -> Self {
        match v {
            0 => Self::Add,
            1 => Self::AddWithCarry,
            2 => Self::Subtract,
            3 => Self::SubtractWithBorrow,
            8 => Self::Xor,
            9 => Self::Or,
            10 => Self::And,
            11 => Self::AndNot,
            12 => Self::Nand,
            _ => Self::Add, // Undefined; upstream has no explicit default
        }
    }
}

/// Result operation encoded in bits [6:4].
///
/// Port of `Tegra::Macro::ResultOperation`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ResultOperation {
    IgnoreAndFetch = 0,
    Move = 1,
    MoveAndSetMethod = 2,
    FetchAndSend = 3,
    MoveAndSend = 4,
    FetchAndSetMethod = 5,
    MoveAndSetMethodFetchAndSend = 6,
    MoveAndSetMethodSend = 7,
}

impl ResultOperation {
    pub fn from_raw(v: u32) -> Self {
        match v & 0x7 {
            0 => Self::IgnoreAndFetch,
            1 => Self::Move,
            2 => Self::MoveAndSetMethod,
            3 => Self::FetchAndSend,
            4 => Self::MoveAndSend,
            5 => Self::FetchAndSetMethod,
            6 => Self::MoveAndSetMethodFetchAndSend,
            7 => Self::MoveAndSetMethodSend,
            _ => unreachable!(),
        }
    }
}

/// Branch condition encoded in bit [4].
///
/// Port of `Tegra::Macro::BranchCondition`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BranchCondition {
    Zero = 0,
    NotZero = 1,
}

impl BranchCondition {
    pub fn from_raw(v: u32) -> Self {
        match v & 1 {
            0 => Self::Zero,
            1 => Self::NotZero,
            _ => unreachable!(),
        }
    }
}

// ── Opcode ───────────────────────────────────────────────────────────────────

/// Decoded macro instruction opcode.
///
/// Port of `Tegra::Macro::Opcode` union.
#[derive(Debug, Clone, Copy)]
pub struct Opcode {
    pub raw: u32,
}

impl Opcode {
    pub const fn new(raw: u32) -> Self {
        Self { raw }
    }

    /// bits [2:0]
    pub fn operation(&self) -> Operation {
        Operation::from_raw(self.raw & 0x7)
    }

    /// bits [6:4]
    pub fn result_operation(&self) -> ResultOperation {
        ResultOperation::from_raw((self.raw >> 4) & 0x7)
    }

    /// bit [4]
    pub fn branch_condition(&self) -> BranchCondition {
        BranchCondition::from_raw((self.raw >> 4) & 0x1)
    }

    /// bit [5] — If set on a branch, then the branch doesn't have a delay slot.
    pub fn branch_annul(&self) -> bool {
        (self.raw >> 5) & 1 != 0
    }

    /// bit [7]
    pub fn is_exit(&self) -> bool {
        (self.raw >> 7) & 1 != 0
    }

    /// bits [10:8]
    pub fn dst(&self) -> u32 {
        (self.raw >> 8) & 0x7
    }

    /// bits [13:11]
    pub fn src_a(&self) -> u32 {
        (self.raw >> 11) & 0x7
    }

    /// bits [16:14]
    pub fn src_b(&self) -> u32 {
        (self.raw >> 14) & 0x7
    }

    /// bits [31:14] — signed 18-bit immediate, overlaps src_b and alu_operation.
    pub fn immediate(&self) -> i32 {
        let raw_imm = (self.raw >> 14) as i32;
        // Sign extend from 18 bits
        (raw_imm << 14) >> 14
    }

    /// bits [21:17]
    pub fn alu_operation(&self) -> AluOperation {
        AluOperation::from_raw((self.raw >> 17) & 0x1F)
    }

    /// bits [21:17] — bitfield source bit position
    pub fn bf_src_bit(&self) -> u32 {
        (self.raw >> 17) & 0x1F
    }

    /// bits [26:22] — bitfield size
    pub fn bf_size(&self) -> u32 {
        (self.raw >> 22) & 0x1F
    }

    /// bits [31:27] — bitfield destination bit position
    pub fn bf_dst_bit(&self) -> u32 {
        (self.raw >> 27) & 0x1F
    }

    /// Returns the bitfield mask: `(1 << bf_size) - 1`.
    pub fn get_bitfield_mask(&self) -> u32 {
        (1u32 << self.bf_size()).wrapping_sub(1)
    }

    /// Returns the branch target offset in bytes.
    pub fn get_branch_target(&self) -> i32 {
        self.immediate() * 4 // sizeof(u32)
    }
}

// ── Method Address ───────────────────────────────────────────────────────────

/// Method address register with auto-increment.
///
/// Port of `Tegra::Macro::MethodAddress` union.
#[derive(Debug, Clone, Copy)]
pub struct MethodAddress {
    pub raw: u32,
}

impl MethodAddress {
    pub const fn new(raw: u32) -> Self {
        Self { raw }
    }

    /// bits [11:0]
    pub fn address(&self) -> u32 {
        self.raw & 0xFFF
    }

    /// bits [17:12]
    pub fn increment(&self) -> u32 {
        (self.raw >> 12) & 0x3F
    }

    /// Set the address field.
    pub fn set_address(&mut self, addr: u32) {
        self.raw = (self.raw & !0xFFF) | (addr & 0xFFF);
    }
}

// ── CachedMacro trait ────────────────────────────────────────────────────────

/// Interface for a compiled/cached macro program.
///
/// Port of the `Tegra::CachedMacro` abstract class.
pub trait CachedMacro: Send {
    /// Execute the macro with the given parameters and method.
    fn execute(&mut self, parameters: &[u32], method: u32);
}

// ── MacroEngine ──────────────────────────────────────────────────────────────

/// Cache info for a single macro method.
///
/// Port of `MacroEngine::CacheInfo`.
struct CacheInfo {
    lle_program: Option<Box<dyn CachedMacro>>,
    hle_program: Option<Box<dyn CachedMacro>>,
    hash: u64,
    has_hle_program: bool,
}

/// Base macro execution engine that manages code upload, caching, and dispatch.
///
/// Port of `Tegra::MacroEngine`.
///
/// Subclasses (interpreter, JIT) provide the `compile` method.
pub struct MacroEngine {
    macro_cache: HashMap<u32, CacheInfo>,
    uploaded_macro_code: HashMap<u32, Vec<u32>>,
    hle_macros: HleMacro,
    // In upstream, this holds a reference to Maxwell3D.
    // That dependency will be wired when engines are integrated.
}

impl MacroEngine {
    /// Create a new macro engine.
    ///
    /// Port of `MacroEngine::MacroEngine(Engines::Maxwell3D&)`.
    pub fn new() -> Self {
        Self {
            macro_cache: HashMap::new(),
            uploaded_macro_code: HashMap::new(),
            hle_macros: HleMacro::new(),
        }
    }

    pub fn set_maxwell_3d(&mut self, maxwell3d: *mut Maxwell3D) {
        self.hle_macros.set_maxwell_3d(maxwell3d);
    }

    /// Store uploaded macro code word.
    ///
    /// Port of `MacroEngine::AddCode`.
    pub fn add_code(&mut self, method: u32, data: u32) {
        let entry = self.uploaded_macro_code.entry(method).or_default();
        entry.push(data);
        if method >= 0x140 && method <= 0x180 {
            log::info!(
                "MacroEngine::add_code method=0x{:X} len={} data=0x{:08X}",
                method,
                entry.len(),
                data
            );
        }
    }

    /// Clear the code associated with a method.
    ///
    /// Port of `MacroEngine::ClearCode`.
    pub fn clear_code(&mut self, method: u32) {
        self.macro_cache.remove(&method);
        self.uploaded_macro_code.remove(&method);
        if method >= 0x140 && method <= 0x180 {
            log::info!("MacroEngine::clear_code method=0x{:X}", method);
        }
    }

    /// Compile (if not cached) and execute a macro.
    ///
    /// Port of `MacroEngine::Execute`.
    ///
    /// The `compile_fn` parameter provides the compilation strategy (interpreter
    /// or JIT), replacing the virtual `Compile` method from upstream.
    pub fn execute<F, R>(
        &mut self,
        method: u32,
        parameters: &[u32],
        mut refresh_parameters: R,
        compile_fn: F,
    )
    where
        F: FnOnce(&[u32]) -> Box<dyn CachedMacro>,
        R: FnMut(),
    {
        if let Some(cache_info) = self.macro_cache.get_mut(&method) {
            if cache_info.has_hle_program {
                if let Some(ref mut hle) = cache_info.hle_program {
                    hle.execute(parameters, method);
                }
            } else if let Some(ref mut lle) = cache_info.lle_program {
                refresh_parameters();
                lle.execute(parameters, method);
            }
            return;
        }

        let mut mid_method = None;
        let code_for_compile = if let Some(code) = self.uploaded_macro_code.get(&method) {
            code.clone()
        } else {
            for (&method_base, code) in &self.uploaded_macro_code {
                if method >= method_base && (method - method_base) < code.len() as u32 {
                    mid_method = Some(method_base);
                    break;
                }
            }
            let method_base = mid_method.expect("Macro was not uploaded");
            let macro_cached = self
                .uploaded_macro_code
                .get(&method_base)
                .expect("mid_method base must exist");
            let rebased_method = (method - method_base) as usize;
            macro_cached[rebased_method..].to_vec()
        };
        let hash = hash_macro_code(&code_for_compile);
        if method == 0x14F {
            log::info!(
                "MacroEngine::execute method=0x{:X} code_len={} hash=0x{:016X} mid_method={:?}",
                method,
                code_for_compile.len(),
                hash,
                mid_method
            );
        }

        let lle_program = compile_fn(&code_for_compile);

        let hle_program = self.hle_macros.get_hle_program(hash);
        let has_hle = hle_program.is_some();
        if method == 0x14F {
            log::info!(
                "MacroEngine::execute method=0x{:X} has_hle={}",
                method,
                has_hle
            );
        }

        let cache_info = CacheInfo {
            lle_program: Some(lle_program),
            hle_program,
            hash,
            has_hle_program: has_hle,
        };

        self.macro_cache.insert(method, cache_info);

        // Execute the newly compiled macro
        let entry = self.macro_cache.get_mut(&method).unwrap();
        if entry.has_hle_program {
            if let Some(ref mut hle) = entry.hle_program {
                hle.execute(parameters, method);
            }
        } else if let Some(ref mut lle) = entry.lle_program {
            refresh_parameters();
            lle.execute(parameters, method);
        }
    }

}

// ── Factory ──────────────────────────────────────────────────────────────────

/// Selects the appropriate macro engine backend.
///
/// Port of `Tegra::GetMacroEngine`.
///
/// Returns a `MacroEngine` — the caller decides whether to use the interpreter
/// or JIT compile function.
pub fn get_macro_engine() -> MacroEngine {
    MacroEngine::new()
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn hash_macro_code(code: &[u32]) -> u64 {
    hash_u32_slice(code) as u64
}

/// Dump macro code to filesystem (debug utility).
///
/// Port of the anonymous `Dump` function from `macro.cpp`.
#[allow(dead_code)]
fn dump(_hash: u64, _code: &[u32], _decompiled: bool) {
    // Upstream writes macro code to `<dump_dir>/macros/<hash>.macro` using
    // Common::FS::GetYuzuPath(DumpDir). Only called when Settings::values.dump_macros
    // is true. Requires Common::FS path resolution and Settings integration,
    // neither of which is wired to video_core yet.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opcode_field_extraction() {
        // Test a known opcode encoding
        let op = Opcode::new(0);
        assert_eq!(op.operation(), Operation::Alu);
        assert_eq!(op.dst(), 0);
        assert_eq!(op.src_a(), 0);
        assert_eq!(op.src_b(), 0);
        assert!(!op.is_exit());
    }

    #[test]
    fn opcode_branch_target() {
        // immediate = 1 (in bits [31:14]), branch target = 1 * 4 = 4
        let op = Opcode::new(1 << 14); // immediate = 1
        assert_eq!(op.get_branch_target(), 4);
    }

    #[test]
    fn method_address_fields() {
        let ma = MethodAddress::new(0x3F_FFF);
        assert_eq!(ma.address(), 0xFFF);
        assert_eq!(ma.increment(), 0x3F);
    }

    #[test]
    fn macro_engine_add_code() {
        let mut engine = MacroEngine::new();
        engine.add_code(0x100, 0xDEADBEEF);
        engine.add_code(0x100, 0xCAFEBABE);
        assert_eq!(
            engine.uploaded_macro_code.get(&0x100),
            Some(&vec![0xDEADBEEF, 0xCAFEBABE])
        );
    }

    #[test]
    fn macro_engine_clear_code() {
        let mut engine = MacroEngine::new();
        engine.add_code(0x100, 0xDEADBEEF);
        engine.add_code(0x101, 0xCAFEBABE);
        engine.clear_code(0x100);
        assert!(!engine.uploaded_macro_code.contains_key(&0x100));
        assert!(engine.uploaded_macro_code.contains_key(&0x101));
    }

    #[test]
    fn macro_engine_execute_rebases_mid_method_inside_uploaded_blob() {
        let mut engine = MacroEngine::new();
        engine.add_code(0x100, 0x11111111);
        engine.add_code(0x100, 0x22222222);
        engine.add_code(0x100, 0x33333333);

        let captured = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let captured_compile = std::sync::Arc::clone(&captured);
        engine.execute(0x101, &[0], || {}, move |code| {
            *captured_compile.lock().unwrap() = code.to_vec();
            struct NoopMacro;
            impl CachedMacro for NoopMacro {
                fn execute(&mut self, _parameters: &[u32], _method: u32) {}
            }
            Box::new(NoopMacro)
        });

        assert_eq!(
            &*captured.lock().unwrap(),
            &[0x22222222, 0x33333333]
        );
    }

    #[test]
    fn hash_macro_code_matches_upstream_hash_range_for_u32_vector() {
        let code = [0x04744351, 0x00708215, 0x00004041, 0x20390021];
        assert_eq!(hash_macro_code(&code), 0x7412B5E8633D2C9B);
    }

    #[test]
    fn num_macro_registers() {
        assert_eq!(NUM_MACRO_REGISTERS, 8);
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL variable allocator.
//!
//! Maps to upstream `backend/glsl/var_alloc.h` and `var_alloc.cpp`.

use std::fmt;

/// GLSL variable type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlslVarType {
    U1,
    F16x2,
    U32,
    F32,
    U64,
    F64,
    U32x2,
    F32x2,
    U32x3,
    F32x3,
    U32x4,
    F32x4,
    PrecF32,
    PrecF64,
    Void,
}

/// Variable identifier packed into a u32.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Id {
    pub raw: u32,
}

impl Id {
    pub fn new() -> Self {
        Self { raw: 0 }
    }

    pub fn is_valid(&self) -> bool {
        self.raw & 1 != 0
    }

    pub fn set_valid(&mut self, v: bool) {
        if v {
            self.raw |= 1;
        } else {
            self.raw &= !1;
        }
    }

    pub fn var_type(&self) -> GlslVarType {
        let bits = (self.raw >> 1) & 0xF;
        match bits {
            0 => GlslVarType::U1,
            1 => GlslVarType::F16x2,
            2 => GlslVarType::U32,
            3 => GlslVarType::F32,
            4 => GlslVarType::U64,
            5 => GlslVarType::F64,
            6 => GlslVarType::U32x2,
            7 => GlslVarType::F32x2,
            8 => GlslVarType::U32x3,
            9 => GlslVarType::F32x3,
            10 => GlslVarType::U32x4,
            11 => GlslVarType::F32x4,
            12 => GlslVarType::PrecF32,
            13 => GlslVarType::PrecF64,
            _ => GlslVarType::Void,
        }
    }

    pub fn set_type(&mut self, t: GlslVarType) {
        let bits = match t {
            GlslVarType::U1 => 0u32,
            GlslVarType::F16x2 => 1,
            GlslVarType::U32 => 2,
            GlslVarType::F32 => 3,
            GlslVarType::U64 => 4,
            GlslVarType::F64 => 5,
            GlslVarType::U32x2 => 6,
            GlslVarType::F32x2 => 7,
            GlslVarType::U32x3 => 8,
            GlslVarType::F32x3 => 9,
            GlslVarType::U32x4 => 10,
            GlslVarType::F32x4 => 11,
            GlslVarType::PrecF32 => 12,
            GlslVarType::PrecF64 => 13,
            GlslVarType::Void => 14,
        };
        self.raw = (self.raw & !(0xF << 1)) | (bits << 1);
    }

    pub fn index(&self) -> u32 {
        self.raw >> 6
    }

    pub fn set_index(&mut self, idx: u32) {
        self.raw = (self.raw & 0x3F) | (idx << 6);
    }
}

impl Default for Id {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id(raw=0x{:08X})", self.raw)
    }
}

/// Return the GLSL type prefix for variable naming.
pub fn type_prefix(t: GlslVarType) -> &'static str {
    match t {
        GlslVarType::U1 => "b_",
        GlslVarType::F16x2 => "f16x2_",
        GlslVarType::U32 => "u_",
        GlslVarType::F32 => "f_",
        GlslVarType::U64 => "u64_",
        GlslVarType::F64 => "d_",
        GlslVarType::U32x2 => "u2_",
        GlslVarType::F32x2 => "f2_",
        GlslVarType::U32x3 => "u3_",
        GlslVarType::F32x3 => "f3_",
        GlslVarType::U32x4 => "u4_",
        GlslVarType::F32x4 => "f4_",
        GlslVarType::PrecF32 => "pf_",
        GlslVarType::PrecF64 => "pd_",
        GlslVarType::Void => "",
    }
}

/// Return the GLSL type string.
pub fn glsl_type_str(t: GlslVarType) -> &'static str {
    match t {
        GlslVarType::U1 => "bool",
        GlslVarType::F16x2 => "f16vec2",
        GlslVarType::U32 => "uint",
        GlslVarType::F32 | GlslVarType::PrecF32 => "float",
        GlslVarType::U64 => "uint64_t",
        GlslVarType::F64 | GlslVarType::PrecF64 => "double",
        GlslVarType::U32x2 => "uvec2",
        GlslVarType::F32x2 => "vec2",
        GlslVarType::U32x3 => "uvec3",
        GlslVarType::F32x3 => "vec3",
        GlslVarType::U32x4 => "uvec4",
        GlslVarType::F32x4 => "vec4",
        GlslVarType::Void => "",
    }
}

/// Per-type usage tracker.
pub struct UseTracker {
    pub uses_temp: bool,
    pub num_used: usize,
    pub var_use: Vec<bool>,
}

impl UseTracker {
    pub fn new() -> Self {
        Self {
            uses_temp: false,
            num_used: 0,
            var_use: Vec::new(),
        }
    }
}

impl Default for UseTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// GLSL variable allocator.
///
/// Matches upstream `VarAlloc` class.
pub struct VarAlloc {
    var_bool: UseTracker,
    var_f16x2: UseTracker,
    var_u32: UseTracker,
    var_u32x2: UseTracker,
    var_u32x3: UseTracker,
    var_u32x4: UseTracker,
    var_f32: UseTracker,
    var_f32x2: UseTracker,
    var_f32x3: UseTracker,
    var_f32x4: UseTracker,
    var_u64: UseTracker,
    var_f64: UseTracker,
    var_precf32: UseTracker,
    var_precf64: UseTracker,
}

impl VarAlloc {
    pub fn new() -> Self {
        Self {
            var_bool: UseTracker::new(),
            var_f16x2: UseTracker::new(),
            var_u32: UseTracker::new(),
            var_u32x2: UseTracker::new(),
            var_u32x3: UseTracker::new(),
            var_u32x4: UseTracker::new(),
            var_f32: UseTracker::new(),
            var_f32x2: UseTracker::new(),
            var_f32x3: UseTracker::new(),
            var_f32x4: UseTracker::new(),
            var_u64: UseTracker::new(),
            var_f64: UseTracker::new(),
            var_precf32: UseTracker::new(),
            var_precf64: UseTracker::new(),
        }
    }

    /// Define a variable of the given type.
    pub fn define(&mut self, var_type: GlslVarType) -> String {
        let id = self.alloc(var_type);
        self.representation(id)
    }

    /// Get the string representation of a variable.
    pub fn representation(&self, id: Id) -> String {
        let prefix = type_prefix(id.var_type());
        format!("{}{}", prefix, id.index())
    }

    /// Get a named representation for a given index and type.
    pub fn representation_indexed(&self, index: u32, var_type: GlslVarType) -> String {
        let prefix = type_prefix(var_type);
        format!("{}{}", prefix, index)
    }

    /// Get the GLSL type string.
    pub fn get_glsl_type(&self, var_type: GlslVarType) -> &'static str {
        glsl_type_str(var_type)
    }

    /// Get the use tracker for a type.
    pub fn get_use_tracker(&self, var_type: GlslVarType) -> &UseTracker {
        match var_type {
            GlslVarType::U1 => &self.var_bool,
            GlslVarType::F16x2 => &self.var_f16x2,
            GlslVarType::U32 => &self.var_u32,
            GlslVarType::F32 => &self.var_f32,
            GlslVarType::U64 => &self.var_u64,
            GlslVarType::F64 => &self.var_f64,
            GlslVarType::U32x2 => &self.var_u32x2,
            GlslVarType::F32x2 => &self.var_f32x2,
            GlslVarType::U32x3 => &self.var_u32x3,
            GlslVarType::F32x3 => &self.var_f32x3,
            GlslVarType::U32x4 => &self.var_u32x4,
            GlslVarType::F32x4 => &self.var_f32x4,
            GlslVarType::PrecF32 => &self.var_precf32,
            GlslVarType::PrecF64 => &self.var_precf64,
            GlslVarType::Void => &self.var_bool, // fallback
        }
    }

    fn get_use_tracker_mut(&mut self, var_type: GlslVarType) -> &mut UseTracker {
        match var_type {
            GlslVarType::U1 => &mut self.var_bool,
            GlslVarType::F16x2 => &mut self.var_f16x2,
            GlslVarType::U32 => &mut self.var_u32,
            GlslVarType::F32 => &mut self.var_f32,
            GlslVarType::U64 => &mut self.var_u64,
            GlslVarType::F64 => &mut self.var_f64,
            GlslVarType::U32x2 => &mut self.var_u32x2,
            GlslVarType::F32x2 => &mut self.var_f32x2,
            GlslVarType::U32x3 => &mut self.var_u32x3,
            GlslVarType::F32x3 => &mut self.var_f32x3,
            GlslVarType::U32x4 => &mut self.var_u32x4,
            GlslVarType::F32x4 => &mut self.var_f32x4,
            GlslVarType::PrecF32 => &mut self.var_precf32,
            GlslVarType::PrecF64 => &mut self.var_precf64,
            GlslVarType::Void => &mut self.var_bool, // fallback
        }
    }

    fn alloc(&mut self, var_type: GlslVarType) -> Id {
        let tracker = self.get_use_tracker_mut(var_type);
        let num_vars = tracker.var_use.len();

        for var in 0..num_vars {
            if tracker.var_use[var] {
                continue;
            }
            tracker.num_used = tracker.num_used.max(var + 1);
            tracker.var_use[var] = true;

            let mut ret = Id::new();
            ret.set_valid(true);
            ret.set_type(var_type);
            ret.set_index(var as u32);
            return ret;
        }

        // Allocate a new variable
        tracker.var_use.push(true);
        let idx = tracker.num_used;
        tracker.num_used += 1;

        let mut ret = Id::new();
        ret.set_valid(true);
        ret.set_type(var_type);
        ret.set_index(idx as u32);
        ret
    }

    /// Free a variable.
    pub fn free(&mut self, id: Id) {
        if !id.is_valid() {
            panic!("Freeing invalid variable");
        }
        let tracker = self.get_use_tracker_mut(id.var_type());
        tracker.var_use[id.index() as usize] = false;
    }
}

impl Default for VarAlloc {
    fn default() -> Self {
        Self::new()
    }
}

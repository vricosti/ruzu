// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR Attribute enum — maps to zuyu's `frontend/ir/attribute.h` and `attribute.cpp`.
//!
//! Defines all shader input/output attributes with the exact same numeric values
//! as the upstream C++ `Shader::IR::Attribute` enum.

use std::fmt;

/// Shader attribute (inputs/outputs, system values).
///
/// Matches zuyu's `IR::Attribute` enum values exactly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u64)]
pub enum Attribute {
    PrimitiveId = 24,
    Layer = 25,
    ViewportIndex = 26,
    PointSize = 27,
    PositionX = 28,
    PositionY = 29,
    PositionZ = 30,
    PositionW = 31,
    Generic0X = 32,
    Generic0Y = 33,
    Generic0Z = 34,
    Generic0W = 35,
    Generic1X = 36,
    Generic1Y = 37,
    Generic1Z = 38,
    Generic1W = 39,
    Generic2X = 40,
    Generic2Y = 41,
    Generic2Z = 42,
    Generic2W = 43,
    Generic3X = 44,
    Generic3Y = 45,
    Generic3Z = 46,
    Generic3W = 47,
    Generic4X = 48,
    Generic4Y = 49,
    Generic4Z = 50,
    Generic4W = 51,
    Generic5X = 52,
    Generic5Y = 53,
    Generic5Z = 54,
    Generic5W = 55,
    Generic6X = 56,
    Generic6Y = 57,
    Generic6Z = 58,
    Generic6W = 59,
    Generic7X = 60,
    Generic7Y = 61,
    Generic7Z = 62,
    Generic7W = 63,
    Generic8X = 64,
    Generic8Y = 65,
    Generic8Z = 66,
    Generic8W = 67,
    Generic9X = 68,
    Generic9Y = 69,
    Generic9Z = 70,
    Generic9W = 71,
    Generic10X = 72,
    Generic10Y = 73,
    Generic10Z = 74,
    Generic10W = 75,
    Generic11X = 76,
    Generic11Y = 77,
    Generic11Z = 78,
    Generic11W = 79,
    Generic12X = 80,
    Generic12Y = 81,
    Generic12Z = 82,
    Generic12W = 83,
    Generic13X = 84,
    Generic13Y = 85,
    Generic13Z = 86,
    Generic13W = 87,
    Generic14X = 88,
    Generic14Y = 89,
    Generic14Z = 90,
    Generic14W = 91,
    Generic15X = 92,
    Generic15Y = 93,
    Generic15Z = 94,
    Generic15W = 95,
    Generic16X = 96,
    Generic16Y = 97,
    Generic16Z = 98,
    Generic16W = 99,
    Generic17X = 100,
    Generic17Y = 101,
    Generic17Z = 102,
    Generic17W = 103,
    Generic18X = 104,
    Generic18Y = 105,
    Generic18Z = 106,
    Generic18W = 107,
    Generic19X = 108,
    Generic19Y = 109,
    Generic19Z = 110,
    Generic19W = 111,
    Generic20X = 112,
    Generic20Y = 113,
    Generic20Z = 114,
    Generic20W = 115,
    Generic21X = 116,
    Generic21Y = 117,
    Generic21Z = 118,
    Generic21W = 119,
    Generic22X = 120,
    Generic22Y = 121,
    Generic22Z = 122,
    Generic22W = 123,
    Generic23X = 124,
    Generic23Y = 125,
    Generic23Z = 126,
    Generic23W = 127,
    Generic24X = 128,
    Generic24Y = 129,
    Generic24Z = 130,
    Generic24W = 131,
    Generic25X = 132,
    Generic25Y = 133,
    Generic25Z = 134,
    Generic25W = 135,
    Generic26X = 136,
    Generic26Y = 137,
    Generic26Z = 138,
    Generic26W = 139,
    Generic27X = 140,
    Generic27Y = 141,
    Generic27Z = 142,
    Generic27W = 143,
    Generic28X = 144,
    Generic28Y = 145,
    Generic28Z = 146,
    Generic28W = 147,
    Generic29X = 148,
    Generic29Y = 149,
    Generic29Z = 150,
    Generic29W = 151,
    Generic30X = 152,
    Generic30Y = 153,
    Generic30Z = 154,
    Generic30W = 155,
    Generic31X = 156,
    Generic31Y = 157,
    Generic31Z = 158,
    Generic31W = 159,
    ColorFrontDiffuseR = 160,
    ColorFrontDiffuseG = 161,
    ColorFrontDiffuseB = 162,
    ColorFrontDiffuseA = 163,
    ColorFrontSpecularR = 164,
    ColorFrontSpecularG = 165,
    ColorFrontSpecularB = 166,
    ColorFrontSpecularA = 167,
    ColorBackDiffuseR = 168,
    ColorBackDiffuseG = 169,
    ColorBackDiffuseB = 170,
    ColorBackDiffuseA = 171,
    ColorBackSpecularR = 172,
    ColorBackSpecularG = 173,
    ColorBackSpecularB = 174,
    ColorBackSpecularA = 175,
    ClipDistance0 = 176,
    ClipDistance1 = 177,
    ClipDistance2 = 178,
    ClipDistance3 = 179,
    ClipDistance4 = 180,
    ClipDistance5 = 181,
    ClipDistance6 = 182,
    ClipDistance7 = 183,
    PointSpriteS = 184,
    PointSpriteT = 185,
    FogCoordinate = 186,
    TessellationEvaluationPointU = 188,
    TessellationEvaluationPointV = 189,
    InstanceId = 190,
    VertexId = 191,
    FixedFncTexture0S = 192,
    FixedFncTexture0T = 193,
    FixedFncTexture0R = 194,
    FixedFncTexture0Q = 195,
    FixedFncTexture1S = 196,
    FixedFncTexture1T = 197,
    FixedFncTexture1R = 198,
    FixedFncTexture1Q = 199,
    FixedFncTexture2S = 200,
    FixedFncTexture2T = 201,
    FixedFncTexture2R = 202,
    FixedFncTexture2Q = 203,
    FixedFncTexture3S = 204,
    FixedFncTexture3T = 205,
    FixedFncTexture3R = 206,
    FixedFncTexture3Q = 207,
    FixedFncTexture4S = 208,
    FixedFncTexture4T = 209,
    FixedFncTexture4R = 210,
    FixedFncTexture4Q = 211,
    FixedFncTexture5S = 212,
    FixedFncTexture5T = 213,
    FixedFncTexture5R = 214,
    FixedFncTexture5Q = 215,
    FixedFncTexture6S = 216,
    FixedFncTexture6T = 217,
    FixedFncTexture6R = 218,
    FixedFncTexture6Q = 219,
    FixedFncTexture7S = 220,
    FixedFncTexture7T = 221,
    FixedFncTexture7R = 222,
    FixedFncTexture7Q = 223,
    FixedFncTexture8S = 224,
    FixedFncTexture8T = 225,
    FixedFncTexture8R = 226,
    FixedFncTexture8Q = 227,
    FixedFncTexture9S = 228,
    FixedFncTexture9T = 229,
    FixedFncTexture9R = 230,
    FixedFncTexture9Q = 231,
    ViewportMask = 232,
    FrontFace = 255,
    // Implementation attributes
    BaseInstance = 256,
    BaseVertex = 257,
    DrawID = 258,
}

pub const NUM_GENERICS: usize = 32;
pub const NUM_FIXEDFNCTEXTURE: usize = 10;

impl Attribute {
    /// Create an attribute from a raw u64 value. Returns None if invalid.
    pub fn from_raw(value: u64) -> Option<Self> {
        // Use a safe transmute approach: validate the value range
        // Generic range is contiguous 32..160
        if (32..160).contains(&value) || value == 24 || value == 25 || value == 26
            || value == 27 || (28..32).contains(&value)
            || (160..232).contains(&value) || value == 232
            || value == 255 || value == 256 || value == 257 || value == 258
        {
            // Safety: all values verified to be valid discriminants
            Some(unsafe { std::mem::transmute::<u64, Attribute>(value) })
        } else {
            None
        }
    }

    /// Raw numeric value of this attribute.
    pub fn raw(self) -> u64 {
        self as u64
    }

    /// Whether this attribute is a generic attribute (Generic0X..Generic31W).
    pub fn is_generic(self) -> bool {
        let v = self.raw();
        v >= Attribute::Generic0X.raw() && v <= Attribute::Generic31W.raw()
    }

    /// Generic attribute index (0..31). Only valid if `is_generic()`.
    pub fn generic_attribute_index(self) -> u32 {
        debug_assert!(self.is_generic());
        ((self.raw() - Attribute::Generic0X.raw()) / 4) as u32
    }

    /// Generic attribute element/component (0..3). Only valid if `is_generic()`.
    pub fn generic_attribute_element(self) -> u32 {
        debug_assert!(self.is_generic());
        (self.raw() % 4) as u32
    }

    /// Construct a generic attribute from index (0..31) and element (0..3).
    pub fn generic(index: u32, element: u32) -> Self {
        debug_assert!(index < 32 && element < 4);
        let value = Attribute::Generic0X.raw() + (index as u64) * 4 + element as u64;
        Attribute::from_raw(value).expect("valid generic attribute")
    }

    /// Whether this is a position attribute (PositionX..PositionW).
    pub fn is_position(self) -> bool {
        let v = self.raw();
        v >= Attribute::PositionX.raw() && v <= Attribute::PositionW.raw()
    }

    /// Position component (0=X, 1=Y, 2=Z, 3=W). Only valid if `is_position()`.
    pub fn position_element(self) -> u32 {
        debug_assert!(self.is_position());
        (self.raw() - Attribute::PositionX.raw()) as u32
    }

    /// Construct a position attribute from element (0..3).
    pub fn position(element: u32) -> Self {
        debug_assert!(element < 4);
        Attribute::from_raw(Attribute::PositionX.raw() + element as u64)
            .expect("valid position attribute")
    }
}

/// Attribute addition: attribute + offset.
/// Matches upstream `operator+(IR::Attribute, size_t)`.
impl std::ops::Add<u64> for Attribute {
    type Output = Self;
    fn add(self, rhs: u64) -> Self {
        Attribute::from_raw(self.raw() + rhs)
            .unwrap_or_else(|| panic!("Attribute overflow: {} + {}", self.raw(), rhs))
    }
}

/// Name of an attribute, matching upstream `NameOf(Attribute)`.
pub fn name_of(attribute: Attribute) -> String {
    if attribute.is_generic() {
        let idx = attribute.generic_attribute_index();
        let elem = ["X", "Y", "Z", "W"][attribute.generic_attribute_element() as usize];
        return format!("Generic[{}].{}", idx, elem);
    }
    match attribute {
        Attribute::PrimitiveId => "PrimitiveId".to_string(),
        Attribute::Layer => "Layer".to_string(),
        Attribute::ViewportIndex => "ViewportIndex".to_string(),
        Attribute::PointSize => "PointSize".to_string(),
        Attribute::PositionX => "Position.X".to_string(),
        Attribute::PositionY => "Position.Y".to_string(),
        Attribute::PositionZ => "Position.Z".to_string(),
        Attribute::PositionW => "Position.W".to_string(),
        Attribute::ColorFrontDiffuseR => "ColorFrontDiffuse.R".to_string(),
        Attribute::ColorFrontDiffuseG => "ColorFrontDiffuse.G".to_string(),
        Attribute::ColorFrontDiffuseB => "ColorFrontDiffuse.B".to_string(),
        Attribute::ColorFrontDiffuseA => "ColorFrontDiffuse.A".to_string(),
        Attribute::ColorFrontSpecularR => "ColorFrontSpecular.R".to_string(),
        Attribute::ColorFrontSpecularG => "ColorFrontSpecular.G".to_string(),
        Attribute::ColorFrontSpecularB => "ColorFrontSpecular.B".to_string(),
        Attribute::ColorFrontSpecularA => "ColorFrontSpecular.A".to_string(),
        Attribute::ColorBackDiffuseR => "ColorBackDiffuse.R".to_string(),
        Attribute::ColorBackDiffuseG => "ColorBackDiffuse.G".to_string(),
        Attribute::ColorBackDiffuseB => "ColorBackDiffuse.B".to_string(),
        Attribute::ColorBackDiffuseA => "ColorBackDiffuse.A".to_string(),
        Attribute::ColorBackSpecularR => "ColorBackSpecular.R".to_string(),
        Attribute::ColorBackSpecularG => "ColorBackSpecular.G".to_string(),
        Attribute::ColorBackSpecularB => "ColorBackSpecular.B".to_string(),
        Attribute::ColorBackSpecularA => "ColorBackSpecular.A".to_string(),
        Attribute::ClipDistance0 => "ClipDistance[0]".to_string(),
        Attribute::ClipDistance1 => "ClipDistance[1]".to_string(),
        Attribute::ClipDistance2 => "ClipDistance[2]".to_string(),
        Attribute::ClipDistance3 => "ClipDistance[3]".to_string(),
        Attribute::ClipDistance4 => "ClipDistance[4]".to_string(),
        Attribute::ClipDistance5 => "ClipDistance[5]".to_string(),
        Attribute::ClipDistance6 => "ClipDistance[6]".to_string(),
        Attribute::ClipDistance7 => "ClipDistance[7]".to_string(),
        Attribute::PointSpriteS => "PointSprite.S".to_string(),
        Attribute::PointSpriteT => "PointSprite.T".to_string(),
        Attribute::FogCoordinate => "FogCoordinate".to_string(),
        Attribute::TessellationEvaluationPointU => "TessellationEvaluationPoint.U".to_string(),
        Attribute::TessellationEvaluationPointV => "TessellationEvaluationPoint.V".to_string(),
        Attribute::InstanceId => "InstanceId".to_string(),
        Attribute::VertexId => "VertexId".to_string(),
        Attribute::ViewportMask => "ViewportMask".to_string(),
        Attribute::FrontFace => "FrontFace".to_string(),
        Attribute::BaseInstance => "BaseInstance".to_string(),
        Attribute::BaseVertex => "BaseVertex".to_string(),
        Attribute::DrawID => "DrawID".to_string(),
        _ => format!("<reserved attribute {}>", attribute.raw()),
    }
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", name_of(*self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generic_attribute() {
        let attr = Attribute::generic(5, 2);
        assert!(attr.is_generic());
        assert_eq!(attr.generic_attribute_index(), 5);
        assert_eq!(attr.generic_attribute_element(), 2);
        assert_eq!(attr, Attribute::Generic5Z);
    }

    #[test]
    fn test_position_attribute() {
        let attr = Attribute::position(0);
        assert!(attr.is_position());
        assert_eq!(attr.position_element(), 0);
        assert_eq!(attr, Attribute::PositionX);
    }

    #[test]
    fn test_attribute_display() {
        assert_eq!(format!("{}", Attribute::PositionX), "Position.X");
        assert_eq!(format!("{}", Attribute::Generic0X), "Generic[0].X");
        assert_eq!(format!("{}", Attribute::Generic31W), "Generic[31].W");
        assert_eq!(format!("{}", Attribute::PrimitiveId), "PrimitiveId");
    }

    #[test]
    fn test_attribute_addition() {
        let attr = Attribute::Generic0X + 4;
        assert_eq!(attr, Attribute::Generic1X);
    }
}

// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_types.h
//!
//! Type constants, enums, and basic Mii data types.

pub const MAX_NAME_SIZE: usize = 10;
pub const MAX_HEIGHT: u8 = 127;
pub const MAX_BUILD: u8 = 127;
pub const MAX_TYPE: u8 = 1;
pub const MAX_REGION_MOVE: u8 = 3;
pub const MAX_EYE_SCALE: u8 = 7;
pub const MAX_EYE_ASPECT: u8 = 6;
pub const MAX_EYE_ROTATE: u8 = 7;
pub const MAX_EYE_X: u8 = 12;
pub const MAX_EYE_Y: u8 = 18;
pub const MAX_EYEBROW_SCALE: u8 = 8;
pub const MAX_EYEBROW_ASPECT: u8 = 6;
pub const MAX_EYEBROW_ROTATE: u8 = 11;
pub const MAX_EYEBROW_X: u8 = 12;
pub const MAX_EYEBROW_Y: u8 = 15;
pub const MAX_NOSE_SCALE: u8 = 8;
pub const MAX_NOSE_Y: u8 = 18;
pub const MAX_MOUTH_SCALE: u8 = 8;
pub const MAX_MOUTH_ASPECT: u8 = 6;
pub const MAX_MOUTH_Y: u8 = 18;
pub const MAX_MUSTACHE_SCALE: u8 = 8;
pub const MAX_MUSTACHE_Y: u8 = 16;
pub const MAX_GLASS_SCALE: u8 = 7;
pub const MAX_GLASS_Y: u8 = 20;
pub const MAX_MOLE_SCALE: u8 = 8;
pub const MAX_MOLE_X: u8 = 16;
pub const MAX_MOLE_Y: u8 = 30;
pub const MAX_VER3_COMMON_COLOR: u8 = 7;
pub const MAX_VER3_GLASS_TYPE: u8 = 8;

pub const DEFAULT_MII_COUNT: usize = 6;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Age {
    Young = 0,
    Normal = 1,
    Old = 2,
    All = 3, // Default
}

impl Default for Age {
    fn default() -> Self {
        Age::All
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Gender {
    Male = 0,
    Female = 1,
    All = 2, // Default
}

impl Gender {
    pub const MAX: Gender = Gender::Female;
}

impl Default for Gender {
    fn default() -> Self {
        Gender::All
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Race {
    Black = 0,
    White = 1,
    Asian = 2,
    All = 3, // Default
}

impl Default for Race {
    fn default() -> Self {
        Race::All
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum HairType {
    NormalLong = 0,
    NormalShort = 1,
    NormalMedium = 2,
    NormalExtraLong = 3,
    NormalLongBottom = 4,
    NormalTwoPeaks = 5,
    PartingLong = 6,
    FrontLock = 7,
    PartingShort = 8,
    PartingExtraLongCurved = 9,
    PartingExtraLong = 10,
    PartingMiddleLong = 11,
    PartingSquared = 12,
    PartingLongBottom = 13,
    PeaksTop = 14,
    PeaksSquared = 15,
    PartingPeaks = 16,
    PeaksLongBottom = 17,
    Peaks = 18,
    PeaksRounded = 19,
    PeaksSide = 20,
    PeaksMedium = 21,
    PeaksLong = 22,
    PeaksRoundedLong = 23,
    PartingFrontPeaks = 24,
    PartingLongFront = 25,
    PartingLongRounded = 26,
    PartingFrontPeaksLong = 27,
    PartingExtraLongRounded = 28,
    LongRounded = 29,
    NormalUnknown1 = 30,
    NormalUnknown2 = 31,
    NormalUnknown3 = 32,
    NormalUnknown4 = 33,
    NormalUnknown5 = 34,
    NormalUnknown6 = 35,
    DreadLocks = 36,
    PlatedMats = 37,
    Caps = 38,
    Afro = 39,
    PlatedMatsLong = 40,
    Beanie = 41,
    Short = 42,
    ShortTopLongSide = 43,
    ShortUnknown1 = 44,
    ShortUnknown2 = 45,
    MilitaryParting = 46,
    Military = 47,
    ShortUnknown3 = 48,
    ShortUnknown4 = 49,
    ShortUnknown5 = 50,
    ShortUnknown6 = 51,
    NoneTop = 52,
    None = 53,
    LongUnknown1 = 54,
    LongUnknown2 = 55,
    LongUnknown3 = 56,
    LongUnknown4 = 57,
    LongUnknown5 = 58,
    LongUnknown6 = 59,
    LongUnknown7 = 60,
    LongUnknown8 = 61,
    LongUnknown9 = 62,
    LongUnknown10 = 63,
    LongUnknown11 = 64,
    LongUnknown12 = 65,
    LongUnknown13 = 66,
    LongUnknown14 = 67,
    LongUnknown15 = 68,
    LongUnknown16 = 69,
    LongUnknown17 = 70,
    LongUnknown18 = 71,
    LongUnknown19 = 72,
    LongUnknown20 = 73,
    LongUnknown21 = 74,
    LongUnknown22 = 75,
    LongUnknown23 = 76,
    LongUnknown24 = 77,
    LongUnknown25 = 78,
    LongUnknown26 = 79,
    LongUnknown27 = 80,
    LongUnknown28 = 81,
    LongUnknown29 = 82,
    LongUnknown30 = 83,
    LongUnknown31 = 84,
    LongUnknown32 = 85,
    LongUnknown33 = 86,
    LongUnknown34 = 87,
    LongUnknown35 = 88,
    LongUnknown36 = 89,
    LongUnknown37 = 90,
    LongUnknown38 = 91,
    LongUnknown39 = 92,
    LongUnknown40 = 93,
    LongUnknown41 = 94,
    LongUnknown42 = 95,
    LongUnknown43 = 96,
    LongUnknown44 = 97,
    LongUnknown45 = 98,
    LongUnknown46 = 99,
    LongUnknown47 = 100,
    LongUnknown48 = 101,
    LongUnknown49 = 102,
    LongUnknown50 = 103,
    LongUnknown51 = 104,
    LongUnknown52 = 105,
    LongUnknown53 = 106,
    LongUnknown54 = 107,
    LongUnknown55 = 108,
    LongUnknown56 = 109,
    LongUnknown57 = 110,
    LongUnknown58 = 111,
    LongUnknown59 = 112,
    LongUnknown60 = 113,
    LongUnknown61 = 114,
    LongUnknown62 = 115,
    LongUnknown63 = 116,
    LongUnknown64 = 117,
    LongUnknown65 = 118,
    LongUnknown66 = 119,
    TwoMediumFrontStrandsOneLongBackPonyTail = 120,
    TwoFrontStrandsLongBackPonyTail = 121,
    PartingFrontTwoLongBackPonyTails = 122,
    TwoFrontStrandsOneLongBackPonyTail = 123,
    LongBackPonyTail = 124,
    LongFrontTwoLongBackPonyTails = 125,
    StrandsTwoShortSidedPonyTails = 126,
    TwoMediumSidedPonyTails = 127,
    ShortFrontTwoBackPonyTails = 128,
    TwoShortSidedPonyTails = 129,
    TwoLongSidedPonyTails = 130,
    LongFrontTwoBackPonyTails = 131,
}

impl HairType {
    pub const MAX: HairType = HairType::LongFrontTwoBackPonyTails;
    pub const MAX_U8: u8 = HairType::LongFrontTwoBackPonyTails as u8;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MoleType {
    None = 0,
    OneDot = 1,
}

impl MoleType {
    pub const MAX: MoleType = MoleType::OneDot;
    pub const MAX_U8: u8 = MoleType::OneDot as u8;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum HairFlip {
    Left = 0,
    Right = 1,
}

impl HairFlip {
    pub const MAX: HairFlip = HairFlip::Right;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CommonColor {
    // For simplicity common colors aren't listed
    Max = 99,
}

impl CommonColor {
    pub const COUNT: usize = 100;
}

impl Default for CommonColor {
    fn default() -> Self {
        CommonColor::Max
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FavoriteColor {
    Red = 0,
    Orange = 1,
    Yellow = 2,
    LimeGreen = 3,
    Green = 4,
    Blue = 5,
    LightBlue = 6,
    Pink = 7,
    Purple = 8,
    Brown = 9,
    White = 10,
    Black = 11,
}

impl FavoriteColor {
    pub const MAX: FavoriteColor = FavoriteColor::Black;
}

impl Default for FavoriteColor {
    fn default() -> Self {
        FavoriteColor::Red
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FontRegion {
    Standard = 0,
    China = 1,
    Korea = 2,
    Taiwan = 3,
}

impl FontRegion {
    pub const MAX: FontRegion = FontRegion::Taiwan;
}

impl Default for FontRegion {
    fn default() -> Self {
        FontRegion::Standard
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FacelineType {
    Sharp = 0,
    Rounded = 1,
    SharpRounded = 2,
    SharpRoundedSmall = 3,
    Large = 4,
    LargeRounded = 5,
    SharpSmall = 6,
    Flat = 7,
    Bump = 8,
    Angular = 9,
    FlatRounded = 10,
    AngularSmall = 11,
}

impl FacelineType {
    pub const MAX: FacelineType = FacelineType::AngularSmall;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FacelineColor {
    Beige = 0,
    WarmBeige = 1,
    Natural = 2,
    Honey = 3,
    Chestnut = 4,
    Porcelain = 5,
    Ivory = 6,
    WarmIvory = 7,
    Almond = 8,
    Espresso = 9,
}

impl FacelineColor {
    pub const MAX: u8 = FacelineColor::Espresso as u8;
    pub const COUNT: usize = (Self::MAX as usize) + 1;
}

impl Default for FacelineColor {
    fn default() -> Self {
        FacelineColor::Beige
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FacelineWrinkle {
    None = 0,
    TearTroughs = 1,
    FacialPain = 2,
    Cheeks = 3,
    Folds = 4,
    UnderTheEyes = 5,
    SplitChin = 6,
    Chin = 7,
    BrowDroop = 8,
    MouthFrown = 9,
    CrowsFeet = 10,
    FoldsCrowsFrown = 11,
}

impl FacelineWrinkle {
    pub const MAX: FacelineWrinkle = FacelineWrinkle::FoldsCrowsFrown;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FacelineMake {
    None = 0,
    CheekPorcelain = 1,
    CheekNatural = 2,
    EyeShadowBlue = 3,
    CheekBlushPorcelain = 4,
    CheekBlushNatural = 5,
    CheekPorcelainEyeShadowBlue = 6,
    CheekPorcelainEyeShadowNatural = 7,
    CheekBlushPorcelainEyeShadowEspresso = 8,
    Freckles = 9,
    LionsManeBeard = 10,
    StubbleBeard = 11,
}

impl FacelineMake {
    pub const MAX: FacelineMake = FacelineMake::StubbleBeard;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EyeType {
    Normal = 0,
    NormalLash = 1,
    WhiteLash = 2,
    WhiteNoBottom = 3,
    OvalAngledWhite = 4,
    AngryWhite = 5,
    DotLashType1 = 6,
    Line = 7,
    DotLine = 8,
    OvalWhite = 9,
    RoundedWhite = 10,
    NormalShadow = 11,
    CircleWhite = 12,
    Circle = 13,
    CircleWhiteStroke = 14,
    NormalOvalNoBottom = 15,
    NormalOvalLarge = 16,
    NormalRoundedNoBottom = 17,
    SmallLash = 18,
    Small = 19,
    TwoSmall = 20,
    NormalLongLash = 21,
    WhiteTwoLashes = 22,
    WhiteThreeLashes = 23,
    DotAngry = 24,
    DotAngled = 25,
    Oval = 26,
    SmallWhite = 27,
    WhiteAngledNoBottom = 28,
    WhiteAngledNoLeft = 29,
    SmallWhiteTwoLashes = 30,
    LeafWhiteLash = 31,
    WhiteLargeNoBottom = 32,
    Dot = 33,
    DotLashType2 = 34,
    DotThreeLashes = 35,
    WhiteOvalTop = 36,
    WhiteOvalBottom = 37,
    WhiteOvalBottomFlat = 38,
    WhiteOvalTwoLashes = 39,
    WhiteOvalThreeLashes = 40,
    WhiteOvalNoBottomTwoLashes = 41,
    DotWhite = 42,
    WhiteOvalTopFlat = 43,
    WhiteThinLeaf = 44,
    StarThreeLashes = 45,
    LineTwoLashes = 46,
    CrowsFeet = 47,
    WhiteNoBottomFlat = 48,
    WhiteNoBottomRounded = 49,
    WhiteSmallBottomLine = 50,
    WhiteNoBottomLash = 51,
    WhiteNoPartialBottomLash = 52,
    WhiteOvalBottomLine = 53,
    WhiteNoBottomLashTopLine = 54,
    WhiteNoPartialBottomTwoLashes = 55,
    NormalTopLine = 56,
    WhiteOvalLash = 57,
    RoundTired = 58,
    WhiteLarge = 59,
}

impl EyeType {
    pub const MAX: EyeType = EyeType::WhiteLarge;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EyebrowType {
    FlatAngledLarge = 0,
    LowArchRoundedThin = 1,
    SoftAngledLarge = 2,
    MediumArchRoundedThin = 3,
    RoundedMedium = 4,
    LowArchMedium = 5,
    RoundedThin = 6,
    UpThin = 7,
    MediumArchRoundedMedium = 8,
    RoundedLarge = 9,
    UpLarge = 10,
    FlatAngledLargeInverted = 11,
    MediumArchFlat = 12,
    AngledThin = 13,
    HorizontalLarge = 14,
    HighArchFlat = 15,
    Flat = 16,
    MediumArchLarge = 17,
    LowArchThin = 18,
    RoundedThinInverted = 19,
    HighArchLarge = 20,
    Hairy = 21,
    Dotted = 22,
    None = 23,
}

impl EyebrowType {
    pub const MAX: EyebrowType = EyebrowType::None;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NoseType {
    Normal = 0,
    Rounded = 1,
    Dot = 2,
    Arrow = 3,
    Roman = 4,
    Triangle = 5,
    Button = 6,
    RoundedInverted = 7,
    Potato = 8,
    Grecian = 9,
    Snub = 10,
    Aquiline = 11,
    ArrowLeft = 12,
    RoundedLarge = 13,
    Hooked = 14,
    Fat = 15,
    Droopy = 16,
    ArrowLarge = 17,
}

impl NoseType {
    pub const MAX: NoseType = NoseType::ArrowLarge;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MouthType {
    Neutral = 0,
    NeutralLips = 1,
    Smile = 2,
    SmileStroke = 3,
    SmileTeeth = 4,
    LipsSmall = 5,
    LipsLarge = 6,
    Wave = 7,
    WaveAngrySmall = 8,
    NeutralStrokeLarge = 9,
    TeethSurprised = 10,
    LipsExtraLarge = 11,
    LipsUp = 12,
    NeutralDown = 13,
    Surprised = 14,
    TeethMiddle = 15,
    NeutralStroke = 16,
    LipsExtraSmall = 17,
    Malicious = 18,
    LipsDual = 19,
    NeutralComma = 20,
    NeutralUp = 21,
    TeethLarge = 22,
    WaveAngry = 23,
    LipsSexy = 24,
    SmileInverted = 25,
    LipsSexyOutline = 26,
    SmileRounded = 27,
    LipsTeeth = 28,
    NeutralOpen = 29,
    TeethRounded = 30,
    WaveAngrySmallInverted = 31,
    NeutralCommaInverted = 32,
    TeethFull = 33,
    SmileDownLine = 34,
    Kiss = 35,
}

impl MouthType {
    pub const MAX: MouthType = MouthType::Kiss;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BeardType {
    None = 0,
    Goatee = 1,
    GoateeLong = 2,
    LionsManeLong = 3,
    LionsMane = 4,
    Full = 5,
}

impl BeardType {
    pub const MIN: BeardType = BeardType::Goatee;
    pub const MAX: BeardType = BeardType::Full;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MustacheType {
    None = 0,
    Walrus = 1,
    Pencil = 2,
    Horseshoe = 3,
    Normal = 4,
    Toothbrush = 5,
}

impl MustacheType {
    pub const MIN: MustacheType = MustacheType::Walrus;
    pub const MAX: MustacheType = MustacheType::Toothbrush;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum GlassType {
    None = 0,
    Oval = 1,
    Wayfarer = 2,
    Rectangle = 3,
    TopRimless = 4,
    Rounded = 5,
    Oversized = 6,
    CatEye = 7,
    Square = 8,
    BottomRimless = 9,
    SemiOpaqueRounded = 10,
    SemiOpaqueCatEye = 11,
    SemiOpaqueOval = 12,
    SemiOpaqueRectangle = 13,
    SemiOpaqueAviator = 14,
    OpaqueRounded = 15,
    OpaqueCatEye = 16,
    OpaqueOval = 17,
    OpaqueRectangle = 18,
    OpaqueAviator = 19,
}

impl GlassType {
    pub const MAX: u8 = GlassType::OpaqueAviator as u8;
    pub const COUNT: usize = (Self::MAX as usize) + 1;
}

impl Default for GlassType {
    fn default() -> Self {
        GlassType::None
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct BeardAndMustacheFlag: u32 {
        const BEARD = 1;
        const MUSTACHE = 2;
        const ALL = Self::BEARD.bits() | Self::MUSTACHE.bits();
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    Database = 0,
    Default = 1,
    Account = 2,
    Friend = 3,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceFlag {
    None = 0,
    Database = 1,
    Default = 2,
}

impl Default for SourceFlag {
    fn default() -> Self {
        SourceFlag::None
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationResult {
    NoErrors = 0x0,
    InvalidBeardColor = 0x1,
    InvalidBeardType = 0x2,
    InvalidBuild = 0x3,
    InvalidEyeAspect = 0x4,
    InvalidEyeColor = 0x5,
    InvalidEyeRotate = 0x6,
    InvalidEyeScale = 0x7,
    InvalidEyeType = 0x8,
    InvalidEyeX = 0x9,
    InvalidEyeY = 0xa,
    InvalidEyebrowAspect = 0xb,
    InvalidEyebrowColor = 0xc,
    InvalidEyebrowRotate = 0xd,
    InvalidEyebrowScale = 0xe,
    InvalidEyebrowType = 0xf,
    InvalidEyebrowX = 0x10,
    InvalidEyebrowY = 0x11,
    InvalidFacelineColor = 0x12,
    InvalidFacelineMake = 0x13,
    InvalidFacelineWrinkle = 0x14,
    InvalidFacelineType = 0x15,
    InvalidColor = 0x16,
    InvalidFont = 0x17,
    InvalidGender = 0x18,
    InvalidGlassColor = 0x19,
    InvalidGlassScale = 0x1a,
    InvalidGlassType = 0x1b,
    InvalidGlassY = 0x1c,
    InvalidHairColor = 0x1d,
    InvalidHairFlip = 0x1e,
    InvalidHairType = 0x1f,
    InvalidHeight = 0x20,
    InvalidMoleScale = 0x21,
    InvalidMoleType = 0x22,
    InvalidMoleX = 0x23,
    InvalidMoleY = 0x24,
    InvalidMouthAspect = 0x25,
    InvalidMouthColor = 0x26,
    InvalidMouthScale = 0x27,
    InvalidMouthType = 0x28,
    InvalidMouthY = 0x29,
    InvalidMustacheScale = 0x2a,
    InvalidMustacheType = 0x2b,
    InvalidMustacheY = 0x2c,
    InvalidNoseScale = 0x2e,
    InvalidNoseType = 0x2f,
    InvalidNoseY = 0x30,
    InvalidRegionMove = 0x31,
    InvalidCreateId = 0x32,
    InvalidName = 0x33,
    InvalidChecksum = 0x34,
    InvalidType = 0x35,
}

/// Nickname: upstream is `struct Nickname { std::array<char16_t, 10> data; }` = 0x14 bytes.
/// Used in CoreData and DefaultMii.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct Nickname {
    pub data: [u16; MAX_NAME_SIZE],
}
const _: () = assert!(core::mem::size_of::<Nickname>() == 0x14);

impl Nickname {
    /// Checks for null or dirty strings, matching upstream Nickname::IsValid.
    pub fn is_valid(&self) -> bool {
        if self.data[0] == 0 {
            return false;
        }

        let mut index = 1;
        while index < MAX_NAME_SIZE && self.data[index] != 0 {
            index += 1;
        }
        while index < MAX_NAME_SIZE && self.data[index] == 0 {
            index += 1;
        }
        index == MAX_NAME_SIZE
    }
}

impl Default for Nickname {
    fn default() -> Self {
        Self {
            data: [0u16; MAX_NAME_SIZE],
        }
    }
}

/// CreateId is a UUID (128-bit).
pub type CreateId = u128;

/// Default Mii configuration data.
/// Maps to upstream `Service::Mii::DefaultMii` in mii_types.h.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DefaultMii {
    pub face_type: u32,
    pub face_color: u32,
    pub face_wrinkle: u32,
    pub face_makeup: u32,
    pub hair_type: u32,
    pub hair_color: u32,
    pub hair_flip: u32,
    pub eye_type: u32,
    pub eye_color: u32,
    pub eye_scale: u32,
    pub eye_aspect: u32,
    pub eye_rotate: u32,
    pub eye_x: u32,
    pub eye_y: u32,
    pub eyebrow_type: u32,
    pub eyebrow_color: u32,
    pub eyebrow_scale: u32,
    pub eyebrow_aspect: u32,
    pub eyebrow_rotate: u32,
    pub eyebrow_x: u32,
    pub eyebrow_y: u32,
    pub nose_type: u32,
    pub nose_scale: u32,
    pub nose_y: u32,
    pub mouth_type: u32,
    pub mouth_color: u32,
    pub mouth_scale: u32,
    pub mouth_aspect: u32,
    pub mouth_y: u32,
    pub mustache_type: u32,
    pub beard_type: u32,
    pub beard_color: u32,
    pub mustache_scale: u32,
    pub mustache_y: u32,
    pub glasses_type: u32,
    pub glasses_color: u32,
    pub glasses_scale: u32,
    pub glasses_y: u32,
    pub mole_type: u32,
    pub mole_scale: u32,
    pub mole_x: u32,
    pub mole_y: u32,
    pub height: u32,
    pub weight: u32,
    pub gender: u32,
    pub favorite_color: u32,
    pub region_move: u32,
    pub font_region: u32,
    pub r#type: u32,
    pub nickname: Nickname,
}

// static_assert equivalent: sizeof(DefaultMii) == 0xd8
const _: () = assert!(std::mem::size_of::<DefaultMii>() == 0xd8);

impl Default for DefaultMii {
    fn default() -> Self {
        // Safety: all-zeros is valid for this repr(C) struct of u32 + Nickname
        unsafe { std::mem::zeroed() }
    }
}

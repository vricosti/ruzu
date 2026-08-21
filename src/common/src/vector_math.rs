// SPDX-FileCopyrightText: 2014 Tony Wasserka
// SPDX-FileCopyrightText: 2014 Dolphin Emulator Project
// SPDX-License-Identifier: BSD-3-Clause AND GPL-2.0-or-later

use std::ops::{
    Add, AddAssign, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Sub, SubAssign,
};

// ---- Vec2 ----

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Vec2<T> {
    pub x: T,
    pub y: T,
}

impl<T> Vec2<T> {
    pub const fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

impl<T: Copy> Vec2<T> {
    pub fn assign_to_all(f: T) -> Self {
        Self { x: f, y: f }
    }

    pub fn cast<T2: From<T>>(self) -> Vec2<T2> {
        Vec2 {
            x: T2::from(self.x),
            y: T2::from(self.y),
        }
    }

    // Aliases: UV (texel coordinates), ST (texture coordinates)
    pub fn u(&self) -> &T {
        &self.x
    }
    pub fn v(&self) -> &T {
        &self.y
    }
    pub fn s(&self) -> &T {
        &self.x
    }
    pub fn t(&self) -> &T {
        &self.y
    }

    pub fn u_mut(&mut self) -> &mut T {
        &mut self.x
    }
    pub fn v_mut(&mut self) -> &mut T {
        &mut self.y
    }
    pub fn s_mut(&mut self) -> &mut T {
        &mut self.x
    }
    pub fn t_mut(&mut self) -> &mut T {
        &mut self.y
    }

    // Swizzlers
    pub fn yx(&self) -> Vec2<T> {
        Vec2 {
            x: self.y,
            y: self.x,
        }
    }
    pub fn vu(&self) -> Vec2<T> {
        Vec2 {
            x: self.y,
            y: self.x,
        }
    }
    pub fn ts(&self) -> Vec2<T> {
        Vec2 {
            x: self.y,
            y: self.x,
        }
    }
}

impl<T: Copy + Default + Mul<Output = T> + Add<Output = T>> Vec2<T> {
    pub fn length2(&self) -> T {
        self.x * self.x + self.y * self.y
    }
}

impl Vec2<f32> {
    pub fn length(&self) -> f32 {
        (self.x * self.x + self.y * self.y).sqrt()
    }

    /// Normalizes in place and returns the previous length.
    pub fn normalize(&mut self) -> f32 {
        let length = self.length();
        self.x /= length;
        self.y /= length;
        length
    }
}

impl<T: Default> Vec2<T> {
    pub fn set_zero(&mut self) {
        self.x = T::default();
        self.y = T::default();
    }
}

impl<T: Copy> Index<usize> for Vec2<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        match i {
            0 => &self.x,
            1 => &self.y,
            _ => panic!("Vec2 index out of range: {}", i),
        }
    }
}

impl<T: Copy> IndexMut<usize> for Vec2<T> {
    fn index_mut(&mut self, i: usize) -> &mut T {
        match i {
            0 => &mut self.x,
            1 => &mut self.y,
            _ => panic!("Vec2 index out of range: {}", i),
        }
    }
}

impl<T: Add<Output = T> + Copy> Add for Vec2<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<T: Add<Output = T> + Copy> AddAssign for Vec2<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.x = self.x + rhs.x;
        self.y = self.y + rhs.y;
    }
}

impl<T: Sub<Output = T> + Copy> Sub for Vec2<T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl<T: Sub<Output = T> + Copy> SubAssign for Vec2<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x = self.x - rhs.x;
        self.y = self.y - rhs.y;
    }
}

impl<T: Neg<Output = T> + Copy> Neg for Vec2<T> {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
        }
    }
}

// Vec2 * Vec2 (component-wise)
impl<T: Mul<Output = T> + Copy> Mul for Vec2<T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
        }
    }
}

// Vec2 * scalar
impl<T: Mul<Output = T> + Copy> Mul<T> for Vec2<T> {
    type Output = Self;
    fn mul(self, f: T) -> Self {
        Self {
            x: self.x * f,
            y: self.y * f,
        }
    }
}

impl<T: Mul<Output = T> + Copy> MulAssign<T> for Vec2<T> {
    fn mul_assign(&mut self, f: T) {
        self.x = self.x * f;
        self.y = self.y * f;
    }
}

impl<T: Div<Output = T> + Copy> Div<T> for Vec2<T> {
    type Output = Self;
    fn div(self, f: T) -> Self {
        Self {
            x: self.x / f,
            y: self.y / f,
        }
    }
}

impl<T: Div<Output = T> + Copy> DivAssign<T> for Vec2<T> {
    fn div_assign(&mut self, f: T) {
        self.x = self.x / f;
        self.y = self.y / f;
    }
}

pub type Vec2f = Vec2<f32>;

// ---- Vec3 ----

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Vec3<T> {
    pub x: T,
    pub y: T,
    pub z: T,
}

impl<T> Vec3<T> {
    pub const fn new(x: T, y: T, z: T) -> Self {
        Self { x, y, z }
    }
}

impl<T: Copy> Vec3<T> {
    pub fn assign_to_all(f: T) -> Self {
        Self { x: f, y: f, z: f }
    }

    pub fn cast<T2: From<T>>(self) -> Vec3<T2> {
        Vec3 {
            x: T2::from(self.x),
            y: T2::from(self.y),
            z: T2::from(self.z),
        }
    }

    // Aliases: UVW, RGB, STQ
    pub fn u(&self) -> &T {
        &self.x
    }
    pub fn v(&self) -> &T {
        &self.y
    }
    pub fn w(&self) -> &T {
        &self.z
    }
    pub fn r(&self) -> &T {
        &self.x
    }
    pub fn g(&self) -> &T {
        &self.y
    }
    pub fn b(&self) -> &T {
        &self.z
    }
    pub fn s(&self) -> &T {
        &self.x
    }
    pub fn t(&self) -> &T {
        &self.y
    }
    pub fn q(&self) -> &T {
        &self.z
    }

    pub fn u_mut(&mut self) -> &mut T {
        &mut self.x
    }
    pub fn v_mut(&mut self) -> &mut T {
        &mut self.y
    }
    pub fn w_mut(&mut self) -> &mut T {
        &mut self.z
    }
    pub fn r_mut(&mut self) -> &mut T {
        &mut self.x
    }
    pub fn g_mut(&mut self) -> &mut T {
        &mut self.y
    }
    pub fn b_mut(&mut self) -> &mut T {
        &mut self.z
    }
    pub fn s_mut(&mut self) -> &mut T {
        &mut self.x
    }
    pub fn t_mut(&mut self) -> &mut T {
        &mut self.y
    }
    pub fn q_mut(&mut self) -> &mut T {
        &mut self.z
    }

    // 2-component swizzlers
    pub fn xy(&self) -> Vec2<T> {
        Vec2::new(self.x, self.y)
    }
    pub fn xz(&self) -> Vec2<T> {
        Vec2::new(self.x, self.z)
    }
    pub fn yz(&self) -> Vec2<T> {
        Vec2::new(self.y, self.z)
    }
    pub fn yx(&self) -> Vec2<T> {
        Vec2::new(self.y, self.x)
    }
    pub fn zx(&self) -> Vec2<T> {
        Vec2::new(self.z, self.x)
    }
    pub fn zy(&self) -> Vec2<T> {
        Vec2::new(self.z, self.y)
    }
    pub fn rg(&self) -> Vec2<T> {
        Vec2::new(self.x, self.y)
    }
    pub fn rb(&self) -> Vec2<T> {
        Vec2::new(self.x, self.z)
    }
    pub fn gb(&self) -> Vec2<T> {
        Vec2::new(self.y, self.z)
    }
    pub fn gr(&self) -> Vec2<T> {
        Vec2::new(self.y, self.x)
    }
    pub fn br(&self) -> Vec2<T> {
        Vec2::new(self.z, self.x)
    }
    pub fn bg(&self) -> Vec2<T> {
        Vec2::new(self.z, self.y)
    }
    pub fn uv(&self) -> Vec2<T> {
        Vec2::new(self.x, self.y)
    }
    pub fn uw(&self) -> Vec2<T> {
        Vec2::new(self.x, self.z)
    }
    pub fn vw(&self) -> Vec2<T> {
        Vec2::new(self.y, self.z)
    }
    pub fn vu(&self) -> Vec2<T> {
        Vec2::new(self.y, self.x)
    }
    pub fn wu(&self) -> Vec2<T> {
        Vec2::new(self.z, self.x)
    }
    pub fn wv(&self) -> Vec2<T> {
        Vec2::new(self.z, self.y)
    }
    pub fn st(&self) -> Vec2<T> {
        Vec2::new(self.x, self.y)
    }
    pub fn sq(&self) -> Vec2<T> {
        Vec2::new(self.x, self.z)
    }
    pub fn tq(&self) -> Vec2<T> {
        Vec2::new(self.y, self.z)
    }
    pub fn ts(&self) -> Vec2<T> {
        Vec2::new(self.y, self.x)
    }
    pub fn qs(&self) -> Vec2<T> {
        Vec2::new(self.z, self.x)
    }
    pub fn qt(&self) -> Vec2<T> {
        Vec2::new(self.z, self.y)
    }
}

impl<T: Copy + Default + Mul<Output = T> + Add<Output = T>> Vec3<T> {
    pub fn length2(&self) -> T {
        self.x * self.x + self.y * self.y + self.z * self.z
    }
}

impl Vec3<f32> {
    pub fn length(&self) -> f32 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }

    pub fn normalized(&self) -> Self {
        let len = self.length();
        Self {
            x: self.x / len,
            y: self.y / len,
            z: self.z / len,
        }
    }

    /// Normalizes in place and returns the previous length.
    pub fn normalize(&mut self) -> f32 {
        let length = self.length();
        self.x /= length;
        self.y /= length;
        self.z /= length;
        length
    }

    pub fn rotate_from_origin(&mut self, roll: f32, pitch: f32, yaw: f32) {
        let temp = self.y;
        self.y = roll.cos() * self.y - roll.sin() * self.z;
        self.z = roll.sin() * temp + roll.cos() * self.z;

        let temp = self.x;
        self.x = pitch.cos() * self.x + pitch.sin() * self.z;
        self.z = -pitch.sin() * temp + pitch.cos() * self.z;

        let temp = self.x;
        self.x = yaw.cos() * self.x - yaw.sin() * self.y;
        self.y = yaw.sin() * temp + yaw.cos() * self.y;
    }
}

impl<T: Default> Vec3<T> {
    pub fn set_zero(&mut self) {
        self.x = T::default();
        self.y = T::default();
        self.z = T::default();
    }
}

impl<T: Copy> Index<usize> for Vec3<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        match i {
            0 => &self.x,
            1 => &self.y,
            2 => &self.z,
            _ => panic!("Vec3 index out of range: {}", i),
        }
    }
}

impl<T: Copy> IndexMut<usize> for Vec3<T> {
    fn index_mut(&mut self, i: usize) -> &mut T {
        match i {
            0 => &mut self.x,
            1 => &mut self.y,
            2 => &mut self.z,
            _ => panic!("Vec3 index out of range: {}", i),
        }
    }
}

impl<T: Add<Output = T> + Copy> Add for Vec3<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl<T: Add<Output = T> + Copy> AddAssign for Vec3<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.x = self.x + rhs.x;
        self.y = self.y + rhs.y;
        self.z = self.z + rhs.z;
    }
}

impl<T: Sub<Output = T> + Copy> Sub for Vec3<T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl<T: Sub<Output = T> + Copy> SubAssign for Vec3<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x = self.x - rhs.x;
        self.y = self.y - rhs.y;
        self.z = self.z - rhs.z;
    }
}

impl<T: Neg<Output = T> + Copy> Neg for Vec3<T> {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

// Vec3 * Vec3 (component-wise)
impl<T: Mul<Output = T> + Copy> Mul for Vec3<T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
            z: self.z * rhs.z,
        }
    }
}

// Vec3 * scalar
impl<T: Mul<Output = T> + Copy> Mul<T> for Vec3<T> {
    type Output = Self;
    fn mul(self, f: T) -> Self {
        Self {
            x: self.x * f,
            y: self.y * f,
            z: self.z * f,
        }
    }
}

impl<T: Mul<Output = T> + Copy> MulAssign<T> for Vec3<T> {
    fn mul_assign(&mut self, f: T) {
        self.x = self.x * f;
        self.y = self.y * f;
        self.z = self.z * f;
    }
}

impl<T: Div<Output = T> + Copy> Div<T> for Vec3<T> {
    type Output = Self;
    fn div(self, f: T) -> Self {
        Self {
            x: self.x / f,
            y: self.y / f,
            z: self.z / f,
        }
    }
}

impl<T: Div<Output = T> + Copy> DivAssign<T> for Vec3<T> {
    fn div_assign(&mut self, f: T) {
        self.x = self.x / f;
        self.y = self.y / f;
        self.z = self.z / f;
    }
}

pub type Vec3f = Vec3<f32>;

// ---- Vec4 ----

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Vec4<T> {
    pub x: T,
    pub y: T,
    pub z: T,
    pub w: T,
}

impl<T> Vec4<T> {
    pub const fn new(x: T, y: T, z: T, w: T) -> Self {
        Self { x, y, z, w }
    }
}

impl<T: Copy> Vec4<T> {
    pub fn assign_to_all(f: T) -> Self {
        Self {
            x: f,
            y: f,
            z: f,
            w: f,
        }
    }

    pub fn cast<T2: From<T>>(self) -> Vec4<T2> {
        Vec4 {
            x: T2::from(self.x),
            y: T2::from(self.y),
            z: T2::from(self.z),
            w: T2::from(self.w),
        }
    }

    // Aliases: RGBA
    pub fn r(&self) -> &T {
        &self.x
    }
    pub fn g(&self) -> &T {
        &self.y
    }
    pub fn b(&self) -> &T {
        &self.z
    }
    pub fn a(&self) -> &T {
        &self.w
    }

    pub fn r_mut(&mut self) -> &mut T {
        &mut self.x
    }
    pub fn g_mut(&mut self) -> &mut T {
        &mut self.y
    }
    pub fn b_mut(&mut self) -> &mut T {
        &mut self.z
    }
    pub fn a_mut(&mut self) -> &mut T {
        &mut self.w
    }

    // 2-component swizzlers
    pub fn xy(&self) -> Vec2<T> {
        Vec2::new(self.x, self.y)
    }
    pub fn xz(&self) -> Vec2<T> {
        Vec2::new(self.x, self.z)
    }
    pub fn xw(&self) -> Vec2<T> {
        Vec2::new(self.x, self.w)
    }
    pub fn yz(&self) -> Vec2<T> {
        Vec2::new(self.y, self.z)
    }
    pub fn yw(&self) -> Vec2<T> {
        Vec2::new(self.y, self.w)
    }
    pub fn zw(&self) -> Vec2<T> {
        Vec2::new(self.z, self.w)
    }
    pub fn yx(&self) -> Vec2<T> {
        Vec2::new(self.y, self.x)
    }
    pub fn zx(&self) -> Vec2<T> {
        Vec2::new(self.z, self.x)
    }
    pub fn wx(&self) -> Vec2<T> {
        Vec2::new(self.w, self.x)
    }
    pub fn zy(&self) -> Vec2<T> {
        Vec2::new(self.z, self.y)
    }
    pub fn wy(&self) -> Vec2<T> {
        Vec2::new(self.w, self.y)
    }
    pub fn wz(&self) -> Vec2<T> {
        Vec2::new(self.w, self.z)
    }
    pub fn rg(&self) -> Vec2<T> {
        Vec2::new(self.x, self.y)
    }
    pub fn rb(&self) -> Vec2<T> {
        Vec2::new(self.x, self.z)
    }
    pub fn ra(&self) -> Vec2<T> {
        Vec2::new(self.x, self.w)
    }
    pub fn gb(&self) -> Vec2<T> {
        Vec2::new(self.y, self.z)
    }
    pub fn ga(&self) -> Vec2<T> {
        Vec2::new(self.y, self.w)
    }
    pub fn ba(&self) -> Vec2<T> {
        Vec2::new(self.z, self.w)
    }
    pub fn gr(&self) -> Vec2<T> {
        Vec2::new(self.y, self.x)
    }
    pub fn br(&self) -> Vec2<T> {
        Vec2::new(self.z, self.x)
    }
    pub fn ar(&self) -> Vec2<T> {
        Vec2::new(self.w, self.x)
    }
    pub fn bg(&self) -> Vec2<T> {
        Vec2::new(self.z, self.y)
    }
    pub fn ag(&self) -> Vec2<T> {
        Vec2::new(self.w, self.y)
    }
    pub fn ab(&self) -> Vec2<T> {
        Vec2::new(self.w, self.z)
    }
    // Repeat swizzlers
    pub fn xx(&self) -> Vec2<T> {
        Vec2::new(self.x, self.x)
    }
    pub fn yy(&self) -> Vec2<T> {
        Vec2::new(self.y, self.y)
    }
    pub fn zz(&self) -> Vec2<T> {
        Vec2::new(self.z, self.z)
    }
    pub fn ww(&self) -> Vec2<T> {
        Vec2::new(self.w, self.w)
    }
    pub fn rr(&self) -> Vec2<T> {
        Vec2::new(self.x, self.x)
    }
    pub fn gg(&self) -> Vec2<T> {
        Vec2::new(self.y, self.y)
    }
    pub fn bb(&self) -> Vec2<T> {
        Vec2::new(self.z, self.z)
    }
    pub fn aa(&self) -> Vec2<T> {
        Vec2::new(self.w, self.w)
    }

    // 3-component swizzlers
    pub fn xyz(&self) -> Vec3<T> {
        Vec3::new(self.x, self.y, self.z)
    }
    pub fn xzy(&self) -> Vec3<T> {
        Vec3::new(self.x, self.z, self.y)
    }
    pub fn yxz(&self) -> Vec3<T> {
        Vec3::new(self.y, self.x, self.z)
    }
    pub fn yzx(&self) -> Vec3<T> {
        Vec3::new(self.y, self.z, self.x)
    }
    pub fn zxy(&self) -> Vec3<T> {
        Vec3::new(self.z, self.x, self.y)
    }
    pub fn zyx(&self) -> Vec3<T> {
        Vec3::new(self.z, self.y, self.x)
    }
    pub fn xyw(&self) -> Vec3<T> {
        Vec3::new(self.x, self.y, self.w)
    }
    pub fn xwy(&self) -> Vec3<T> {
        Vec3::new(self.x, self.w, self.y)
    }
    pub fn yxw(&self) -> Vec3<T> {
        Vec3::new(self.y, self.x, self.w)
    }
    pub fn ywx(&self) -> Vec3<T> {
        Vec3::new(self.y, self.w, self.x)
    }
    pub fn wxy(&self) -> Vec3<T> {
        Vec3::new(self.w, self.x, self.y)
    }
    pub fn wyx(&self) -> Vec3<T> {
        Vec3::new(self.w, self.y, self.x)
    }
    pub fn xzw(&self) -> Vec3<T> {
        Vec3::new(self.x, self.z, self.w)
    }
    pub fn xwz(&self) -> Vec3<T> {
        Vec3::new(self.x, self.w, self.z)
    }
    pub fn zxw(&self) -> Vec3<T> {
        Vec3::new(self.z, self.x, self.w)
    }
    pub fn zwx(&self) -> Vec3<T> {
        Vec3::new(self.z, self.w, self.x)
    }
    pub fn wxz(&self) -> Vec3<T> {
        Vec3::new(self.w, self.x, self.z)
    }
    pub fn wzx(&self) -> Vec3<T> {
        Vec3::new(self.w, self.z, self.x)
    }
    pub fn yzw(&self) -> Vec3<T> {
        Vec3::new(self.y, self.z, self.w)
    }
    pub fn ywz(&self) -> Vec3<T> {
        Vec3::new(self.y, self.w, self.z)
    }
    pub fn zyw(&self) -> Vec3<T> {
        Vec3::new(self.z, self.y, self.w)
    }
    pub fn zwy(&self) -> Vec3<T> {
        Vec3::new(self.z, self.w, self.y)
    }
    pub fn wyz(&self) -> Vec3<T> {
        Vec3::new(self.w, self.y, self.z)
    }
    pub fn wzy(&self) -> Vec3<T> {
        Vec3::new(self.w, self.z, self.y)
    }
    pub fn rgb(&self) -> Vec3<T> {
        Vec3::new(self.x, self.y, self.z)
    }
    pub fn rga(&self) -> Vec3<T> {
        Vec3::new(self.x, self.y, self.w)
    }
    pub fn rba(&self) -> Vec3<T> {
        Vec3::new(self.x, self.z, self.w)
    }
    pub fn gba(&self) -> Vec3<T> {
        Vec3::new(self.y, self.z, self.w)
    }
    // Repeat 3-component swizzlers
    pub fn xxx(&self) -> Vec3<T> {
        Vec3::new(self.x, self.x, self.x)
    }
    pub fn yyy(&self) -> Vec3<T> {
        Vec3::new(self.y, self.y, self.y)
    }
    pub fn zzz(&self) -> Vec3<T> {
        Vec3::new(self.z, self.z, self.z)
    }
    pub fn www(&self) -> Vec3<T> {
        Vec3::new(self.w, self.w, self.w)
    }
    pub fn rrr(&self) -> Vec3<T> {
        Vec3::new(self.x, self.x, self.x)
    }
    pub fn ggg(&self) -> Vec3<T> {
        Vec3::new(self.y, self.y, self.y)
    }
    pub fn bbb(&self) -> Vec3<T> {
        Vec3::new(self.z, self.z, self.z)
    }
    pub fn aaa(&self) -> Vec3<T> {
        Vec3::new(self.w, self.w, self.w)
    }
}

impl<T: Copy + Default + Mul<Output = T> + Add<Output = T>> Vec4<T> {
    pub fn length2(&self) -> T {
        self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w
    }
}

impl<T: Default> Vec4<T> {
    pub fn set_zero(&mut self) {
        self.x = T::default();
        self.y = T::default();
        self.z = T::default();
        self.w = T::default();
    }
}

impl<T: Copy> Index<usize> for Vec4<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        match i {
            0 => &self.x,
            1 => &self.y,
            2 => &self.z,
            3 => &self.w,
            _ => panic!("Vec4 index out of range: {}", i),
        }
    }
}

impl<T: Copy> IndexMut<usize> for Vec4<T> {
    fn index_mut(&mut self, i: usize) -> &mut T {
        match i {
            0 => &mut self.x,
            1 => &mut self.y,
            2 => &mut self.z,
            3 => &mut self.w,
            _ => panic!("Vec4 index out of range: {}", i),
        }
    }
}

impl<T: Add<Output = T> + Copy> Add for Vec4<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
            w: self.w + rhs.w,
        }
    }
}

impl<T: Add<Output = T> + Copy> AddAssign for Vec4<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.x = self.x + rhs.x;
        self.y = self.y + rhs.y;
        self.z = self.z + rhs.z;
        self.w = self.w + rhs.w;
    }
}

impl<T: Sub<Output = T> + Copy> Sub for Vec4<T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
            w: self.w - rhs.w,
        }
    }
}

impl<T: Sub<Output = T> + Copy> SubAssign for Vec4<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x = self.x - rhs.x;
        self.y = self.y - rhs.y;
        self.z = self.z - rhs.z;
        self.w = self.w - rhs.w;
    }
}

impl<T: Neg<Output = T> + Copy> Neg for Vec4<T> {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
            w: -self.w,
        }
    }
}

// Vec4 * Vec4 (component-wise)
impl<T: Mul<Output = T> + Copy> Mul for Vec4<T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
            z: self.z * rhs.z,
            w: self.w * rhs.w,
        }
    }
}

// Vec4 * scalar
impl<T: Mul<Output = T> + Copy> Mul<T> for Vec4<T> {
    type Output = Self;
    fn mul(self, f: T) -> Self {
        Self {
            x: self.x * f,
            y: self.y * f,
            z: self.z * f,
            w: self.w * f,
        }
    }
}

impl<T: Mul<Output = T> + Copy> MulAssign<T> for Vec4<T> {
    fn mul_assign(&mut self, f: T) {
        self.x = self.x * f;
        self.y = self.y * f;
        self.z = self.z * f;
        self.w = self.w * f;
    }
}

impl<T: Div<Output = T> + Copy> Div<T> for Vec4<T> {
    type Output = Self;
    fn div(self, f: T) -> Self {
        Self {
            x: self.x / f,
            y: self.y / f,
            z: self.z / f,
            w: self.w / f,
        }
    }
}

impl<T: Div<Output = T> + Copy> DivAssign<T> for Vec4<T> {
    fn div_assign(&mut self, f: T) {
        self.x = self.x / f;
        self.y = self.y / f;
        self.z = self.z / f;
        self.w = self.w / f;
    }
}

pub type Vec4f = Vec4<f32>;

// ---- Free functions ----

pub fn dot_vec2<T: Copy + Mul<Output = T> + Add<Output = T>>(a: &Vec2<T>, b: &Vec2<T>) -> T {
    a.x * b.x + a.y * b.y
}

pub fn dot_vec3<T: Copy + Mul<Output = T> + Add<Output = T>>(a: &Vec3<T>, b: &Vec3<T>) -> T {
    a.x * b.x + a.y * b.y + a.z * b.z
}

pub fn dot_vec4<T: Copy + Mul<Output = T> + Add<Output = T>>(a: &Vec4<T>, b: &Vec4<T>) -> T {
    a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
}

pub fn cross<T: Copy + Mul<Output = T> + Sub<Output = T>>(a: &Vec3<T>, b: &Vec3<T>) -> Vec3<T> {
    Vec3 {
        x: a.y * b.z - a.z * b.y,
        y: a.z * b.x - a.x * b.z,
        z: a.x * b.y - a.y * b.x,
    }
}

/// Linear interpolation via float: 0.0=begin, 1.0=end
pub fn lerp<X>(begin: X, end: X, t: f32) -> X
where
    X: Mul<f32, Output = X> + Add<Output = X> + Copy,
{
    begin * (1.0f32 - t) + end * t
}

/// Linear interpolation via int: 0=begin, base=end
pub fn lerp_int<X>(begin: X, end: X, t: i32, base: i32) -> X
where
    X: Mul<i32, Output = X> + Add<Output = X> + Div<i32, Output = X> + Copy,
{
    (begin * (base - t) + end * t) / base
}

/// Bilinear interpolation. s is for interpolating x00-x01 and x10-x11,
/// and t is for the second interpolation.
pub fn bilinear_interp<X>(x00: X, x01: X, x10: X, x11: X, s: f32, t: f32) -> X
where
    X: Mul<f32, Output = X> + Add<Output = X> + Copy,
{
    let y0 = lerp(x00, x01, s);
    let y1 = lerp(x10, x11, s);
    lerp(y0, y1, t)
}

// Utility vector factories

pub fn make_vec2<T>(x: T, y: T) -> Vec2<T> {
    Vec2 { x, y }
}

pub fn make_vec3<T>(x: T, y: T, z: T) -> Vec3<T> {
    Vec3 { x, y, z }
}

pub fn make_vec3_from_vec2<T: Copy>(xy: &Vec2<T>, z: T) -> Vec3<T> {
    Vec3 {
        x: xy[0],
        y: xy[1],
        z,
    }
}

pub fn make_vec3_from_scalar_vec2<T: Copy>(x: T, yz: &Vec2<T>) -> Vec3<T> {
    Vec3 {
        x,
        y: yz[0],
        z: yz[1],
    }
}

pub fn make_vec4<T>(x: T, y: T, z: T, w: T) -> Vec4<T> {
    Vec4 { x, y, z, w }
}

pub fn make_vec4_from_vec2s<T: Copy>(xy: &Vec2<T>, zw: &Vec2<T>) -> Vec4<T> {
    Vec4 {
        x: xy[0],
        y: xy[1],
        z: zw[0],
        w: zw[1],
    }
}

pub fn make_vec4_from_vec3<T: Copy>(xyz: &Vec3<T>, w: T) -> Vec4<T> {
    Vec4 {
        x: xyz[0],
        y: xyz[1],
        z: xyz[2],
        w,
    }
}

pub fn make_vec4_from_scalar_vec3<T: Copy>(x: T, yzw: &Vec3<T>) -> Vec4<T> {
    Vec4 {
        x,
        y: yzw[0],
        z: yzw[1],
        w: yzw[2],
    }
}

pub fn make_vec4_from_vec2_scalars<T: Copy>(xy: &Vec2<T>, z: T, w: T) -> Vec4<T> {
    Vec4 {
        x: xy[0],
        y: xy[1],
        z,
        w,
    }
}

pub fn make_vec4_from_scalar_vec2_scalar<T: Copy>(x: T, yz: &Vec2<T>, w: T) -> Vec4<T> {
    Vec4 {
        x,
        y: yz[0],
        z: yz[1],
        w,
    }
}

pub fn make_vec4_from_scalars_vec2<T: Copy>(x: T, y: T, zw: &Vec2<T>) -> Vec4<T> {
    Vec4 {
        x,
        y,
        z: zw[0],
        w: zw[1],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec2_basic() {
        let a = Vec2::new(1.0f32, 2.0);
        let b = Vec2::new(3.0, 4.0);
        assert_eq!(a + b, Vec2::new(4.0, 6.0));
        assert_eq!(a - b, Vec2::new(-2.0, -2.0));
        assert_eq!(-a, Vec2::new(-1.0, -2.0));
        assert_eq!(a * 2.0, Vec2::new(2.0, 4.0));
        assert_eq!(a / 2.0, Vec2::new(0.5, 1.0));
    }

    #[test]
    fn test_vec2_length() {
        let v = Vec2::new(3.0f32, 4.0);
        assert!((v.length() - 5.0).abs() < 1e-6);
        assert_eq!(v.length2(), 25.0);
    }

    #[test]
    fn test_vec3_basic() {
        let a = Vec3::new(1.0f32, 2.0, 3.0);
        let b = Vec3::new(4.0, 5.0, 6.0);
        assert_eq!(a + b, Vec3::new(5.0, 7.0, 9.0));
        assert_eq!(a * 2.0, Vec3::new(2.0, 4.0, 6.0));
    }

    #[test]
    fn test_vec3_cross() {
        let a = Vec3::new(1.0f32, 0.0, 0.0);
        let b = Vec3::new(0.0, 1.0, 0.0);
        let c = cross(&a, &b);
        assert_eq!(c, Vec3::new(0.0, 0.0, 1.0));
    }

    #[test]
    fn test_vec3_dot() {
        let a = Vec3::new(1.0f32, 2.0, 3.0);
        let b = Vec3::new(4.0, 5.0, 6.0);
        assert_eq!(dot_vec3(&a, &b), 32.0);
    }

    #[test]
    fn test_vec4_basic() {
        let a = Vec4::new(1.0f32, 2.0, 3.0, 4.0);
        let b = Vec4::new(5.0, 6.0, 7.0, 8.0);
        assert_eq!(a + b, Vec4::new(6.0, 8.0, 10.0, 12.0));
    }

    #[test]
    fn test_vec_index() {
        let v = Vec3::new(10, 20, 30);
        assert_eq!(v[0], 10);
        assert_eq!(v[1], 20);
        assert_eq!(v[2], 30);
    }

    #[test]
    fn test_lerp() {
        let result = lerp(Vec2::new(0.0f32, 0.0), Vec2::new(10.0, 10.0), 0.5);
        assert!((result.x - 5.0).abs() < 1e-6);
        assert!((result.y - 5.0).abs() < 1e-6);
    }

    #[test]
    fn test_make_vec4_from_vec3() {
        let xyz = Vec3::new(1.0f32, 2.0, 3.0);
        let v = make_vec4_from_vec3(&xyz, 4.0);
        assert_eq!(v, Vec4::new(1.0, 2.0, 3.0, 4.0));
    }

    #[test]
    fn test_swizzlers() {
        let v = Vec3::new(1, 2, 3);
        assert_eq!(v.xy(), Vec2::new(1, 2));
        assert_eq!(v.yz(), Vec2::new(2, 3));
        assert_eq!(v.zx(), Vec2::new(3, 1));

        let v4 = Vec4::new(1, 2, 3, 4);
        assert_eq!(v4.xyz(), Vec3::new(1, 2, 3));
        assert_eq!(v4.zw(), Vec2::new(3, 4));
    }
}

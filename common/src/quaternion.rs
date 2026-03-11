// SPDX-FileCopyrightText: 2016 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

use crate::vector_math::{cross, dot_vec3, Vec3};
use std::ops::{Add, Mul, Neg, Sub};

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Quaternion<T> {
    pub xyz: Vec3<T>,
    pub w: T,
}

impl<T> Quaternion<T> {
    pub const fn new(xyz: Vec3<T>, w: T) -> Self {
        Self { xyz, w }
    }
}

impl<T: Neg<Output = T> + Copy> Quaternion<T> {
    pub fn inverse(&self) -> Self {
        Self {
            xyz: -self.xyz,
            w: self.w,
        }
    }
}

impl<T: Add<Output = T> + Copy> Add for Quaternion<T> {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            xyz: self.xyz + other.xyz,
            w: self.w + other.w,
        }
    }
}

impl<T: Sub<Output = T> + Copy> Sub for Quaternion<T> {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self {
            xyz: self.xyz - other.xyz,
            w: self.w - other.w,
        }
    }
}

/// Quaternion multiplication (Hamilton product).
impl<T> Mul for Quaternion<T>
where
    T: Copy + Mul<Output = T> + Add<Output = T> + Sub<Output = T>,
{
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        // xyz * other.w + other.xyz * w + cross(xyz, other.xyz)
        let term1 = self.xyz * other.w;
        let term2 = other.xyz * self.w;
        let term3 = cross(&self.xyz, &other.xyz);
        Self {
            xyz: term1 + term2 + term3,
            w: self.w * other.w - dot_vec3(&self.xyz, &other.xyz),
        }
    }
}

impl Quaternion<f32> {
    pub fn normalized(&self) -> Self {
        let length = (self.xyz.length2() + self.w * self.w).sqrt();
        Self {
            xyz: self.xyz / length,
            w: self.w / length,
        }
    }

    pub fn to_matrix(&self) -> [f32; 16] {
        let x2 = self.xyz[0] * self.xyz[0];
        let y2 = self.xyz[1] * self.xyz[1];
        let z2 = self.xyz[2] * self.xyz[2];

        let xy = self.xyz[0] * self.xyz[1];
        let wz = self.w * self.xyz[2];
        let xz = self.xyz[0] * self.xyz[2];
        let wy = self.w * self.xyz[1];
        let yz = self.xyz[1] * self.xyz[2];
        let wx = self.w * self.xyz[0];

        [
            1.0 - 2.0 * (y2 + z2),
            2.0 * (xy + wz),
            2.0 * (xz - wy),
            0.0,
            2.0 * (xy - wz),
            1.0 - 2.0 * (x2 + z2),
            2.0 * (yz + wx),
            0.0,
            2.0 * (xz + wy),
            2.0 * (yz - wx),
            1.0 - 2.0 * (x2 + y2),
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
        ]
    }
}

pub fn quaternion_rotate(q: &Quaternion<f32>, v: &Vec3<f32>) -> Vec3<f32> {
    *v + cross(&q.xyz, &(cross(&q.xyz, v) + *v * q.w)) * 2.0
}

pub fn make_quaternion(axis: &Vec3<f32>, angle: f32) -> Quaternion<f32> {
    Quaternion {
        xyz: *axis * (angle / 2.0).sin(),
        w: (angle / 2.0).cos(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quaternion_inverse() {
        let q = Quaternion::new(Vec3::new(1.0f32, 2.0, 3.0), 4.0);
        let inv = q.inverse();
        assert_eq!(inv.xyz, Vec3::new(-1.0, -2.0, -3.0));
        assert_eq!(inv.w, 4.0);
    }

    #[test]
    fn test_quaternion_add_sub() {
        let a = Quaternion::new(Vec3::new(1.0f32, 2.0, 3.0), 4.0);
        let b = Quaternion::new(Vec3::new(5.0, 6.0, 7.0), 8.0);
        let sum = a + b;
        assert_eq!(sum.xyz, Vec3::new(6.0, 8.0, 10.0));
        assert_eq!(sum.w, 12.0);
    }

    #[test]
    fn test_quaternion_identity_matrix() {
        let q = Quaternion::new(Vec3::new(0.0f32, 0.0, 0.0), 1.0);
        let m = q.to_matrix();
        let expected = [
            1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ];
        for (a, b) in m.iter().zip(expected.iter()) {
            assert!((a - b).abs() < 1e-6);
        }
    }

    #[test]
    fn test_quaternion_normalized() {
        let q = Quaternion::new(Vec3::new(1.0f32, 2.0, 3.0), 4.0);
        let n = q.normalized();
        let len = (n.xyz.length2() + n.w * n.w).sqrt();
        assert!((len - 1.0).abs() < 1e-6);
    }
}

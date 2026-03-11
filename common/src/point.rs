// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

/// Represents a point within a 2D space.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Point<T> {
    pub x: T,
    pub y: T,
}

impl<T> Point<T> {
    pub const fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

// Point op Point
impl<T: Add<Output = T> + Copy> Add for Point<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<T: Add<Output = T> + Copy> AddAssign for Point<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.x = self.x + rhs.x;
        self.y = self.y + rhs.y;
    }
}

impl<T: Sub<Output = T> + Copy> Sub for Point<T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl<T: Sub<Output = T> + Copy> SubAssign for Point<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x = self.x - rhs.x;
        self.y = self.y - rhs.y;
    }
}

impl<T: Mul<Output = T> + Copy> Mul for Point<T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
        }
    }
}

impl<T: Mul<Output = T> + Copy> MulAssign for Point<T> {
    fn mul_assign(&mut self, rhs: Self) {
        self.x = self.x * rhs.x;
        self.y = self.y * rhs.y;
    }
}

impl<T: Div<Output = T> + Copy> Div for Point<T> {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        Self {
            x: self.x / rhs.x,
            y: self.y / rhs.y,
        }
    }
}

impl<T: Div<Output = T> + Copy> DivAssign for Point<T> {
    fn div_assign(&mut self, rhs: Self) {
        self.x = self.x / rhs.x;
        self.y = self.y / rhs.y;
    }
}

// Point op scalar
impl<T: Mul<Output = T> + Copy> Mul<T> for Point<T> {
    type Output = Self;
    fn mul(self, f: T) -> Self {
        Self {
            x: self.x * f,
            y: self.y * f,
        }
    }
}

impl<T: Mul<Output = T> + Copy> MulAssign<T> for Point<T> {
    fn mul_assign(&mut self, f: T) {
        self.x = self.x * f;
        self.y = self.y * f;
    }
}

impl<T: Div<Output = T> + Copy> Div<T> for Point<T> {
    type Output = Self;
    fn div(self, f: T) -> Self {
        Self {
            x: self.x / f,
            y: self.y / f,
        }
    }
}

impl<T: Div<Output = T> + Copy> DivAssign<T> for Point<T> {
    fn div_assign(&mut self, f: T) {
        self.x = self.x / f;
        self.y = self.y / f;
    }
}

impl<T: Add<Output = T> + Copy> Add<T> for Point<T> {
    type Output = Self;
    fn add(self, f: T) -> Self {
        Self {
            x: self.x + f,
            y: self.y + f,
        }
    }
}

impl<T: Add<Output = T> + Copy> AddAssign<T> for Point<T> {
    fn add_assign(&mut self, f: T) {
        self.x = self.x + f;
        self.y = self.y + f;
    }
}

impl<T: Sub<Output = T> + Copy> Sub<T> for Point<T> {
    type Output = Self;
    fn sub(self, f: T) -> Self {
        Self {
            x: self.x - f,
            y: self.y - f,
        }
    }
}

impl<T: Sub<Output = T> + Copy> SubAssign<T> for Point<T> {
    fn sub_assign(&mut self, f: T) {
        self.x = self.x - f;
        self.y = self.y - f;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_arithmetic() {
        let a = Point::new(1, 2);
        let b = Point::new(3, 4);
        assert_eq!(a + b, Point::new(4, 6));
        assert_eq!(b - a, Point::new(2, 2));
        assert_eq!(a * b, Point::new(3, 8));
        assert_eq!(b / Point::new(1, 2), Point::new(3, 2));
    }

    #[test]
    fn test_point_scalar() {
        let a = Point::new(2, 4);
        assert_eq!(a * 3, Point::new(6, 12));
        assert_eq!(a / 2, Point::new(1, 2));
    }

    #[test]
    fn test_point_assign() {
        let mut a = Point::new(1, 2);
        a += Point::new(3, 4);
        assert_eq!(a, Point::new(4, 6));
        a -= Point::new(1, 1);
        assert_eq!(a, Point::new(3, 5));
    }
}

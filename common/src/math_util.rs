//! Port of zuyu/src/common/math_util.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Math utilities: PI constant and Rectangle struct.

/// Pi constant (32-bit float precision), matching the C++ `Common::PI`.
pub const PI: f32 = 3.1415926535f32;

/// A rectangle defined by left, top, right, bottom edges.
/// Corresponds to `Common::Rectangle<T>` in the C++ code.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Rectangle<T> {
    pub left: T,
    pub top: T,
    pub right: T,
    pub bottom: T,
}

impl<T> Rectangle<T>
where
    T: Copy
        + Default
        + PartialOrd
        + core::ops::Sub<Output = T>
        + core::ops::Add<Output = T>,
{
    /// Create a new rectangle from width and height (left=0, top=0).
    #[inline]
    pub fn from_dimensions(width: T, height: T) -> Self {
        Self {
            left: T::default(),
            top: T::default(),
            right: width,
            bottom: height,
        }
    }

    /// Create a new rectangle from explicit bounds.
    #[inline]
    pub fn new(left: T, top: T, right: T, bottom: T) -> Self {
        Self {
            left,
            top,
            right,
            bottom,
        }
    }

    /// Translate the rectangle along the X axis.
    #[inline]
    pub fn translate_x(self, x: T) -> Self {
        Self {
            left: self.left + x,
            top: self.top,
            right: self.right + x,
            bottom: self.bottom,
        }
    }

    /// Translate the rectangle along the Y axis.
    #[inline]
    pub fn translate_y(self, y: T) -> Self {
        Self {
            left: self.left,
            top: self.top + y,
            right: self.right,
            bottom: self.bottom + y,
        }
    }
}

/// Integer-specific methods (using abs via signed conversion).
impl Rectangle<u32> {
    /// Get the width of the rectangle.
    #[inline]
    pub fn get_width(&self) -> u32 {
        if self.right >= self.left {
            self.right - self.left
        } else {
            self.left - self.right
        }
    }

    /// Get the height of the rectangle.
    #[inline]
    pub fn get_height(&self) -> u32 {
        if self.bottom >= self.top {
            self.bottom - self.top
        } else {
            self.top - self.bottom
        }
    }

    /// Check if the rectangle is empty (zero or negative area).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.right <= self.left || self.bottom <= self.top
    }

    /// Compute the intersection of two rectangles.
    /// Returns `Some(rect)` if intersection is non-empty, `None` otherwise.
    #[inline]
    pub fn intersect(&self, other: &Self) -> Option<Self> {
        let result = Self {
            left: self.left.max(other.left),
            top: self.top.max(other.top),
            right: self.right.min(other.right),
            bottom: self.bottom.min(other.bottom),
        };
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    /// Scale the rectangle by a float factor.
    #[inline]
    pub fn scale(&self, s: f32) -> Self {
        Self {
            left: self.left,
            top: self.top,
            right: ((self.left + self.get_width()) as f32 * s) as u32,
            bottom: ((self.top + self.get_height()) as f32 * s) as u32,
        }
    }
}

/// Float-specific methods.
impl Rectangle<f32> {
    /// Get the width of the rectangle.
    #[inline]
    pub fn get_width(&self) -> f32 {
        (self.right - self.left).abs()
    }

    /// Get the height of the rectangle.
    #[inline]
    pub fn get_height(&self) -> f32 {
        (self.bottom - self.top).abs()
    }

    /// Check if the rectangle is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.get_width() <= 0.0 || self.get_height() <= 0.0
    }

    /// Compute the intersection of two rectangles.
    #[inline]
    pub fn intersect(&self, other: &Self) -> Option<Self> {
        let result = Self {
            left: self.left.max(other.left),
            top: self.top.max(other.top),
            right: self.right.min(other.right),
            bottom: self.bottom.min(other.bottom),
        };
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    /// Scale the rectangle by a float factor.
    #[inline]
    pub fn scale(&self, s: f32) -> Self {
        Self {
            left: self.left,
            top: self.top,
            right: (self.left + self.get_width()) * s,
            bottom: (self.top + self.get_height()) * s,
        }
    }
}

/// i32-specific methods.
impl Rectangle<i32> {
    /// Get the width of the rectangle.
    #[inline]
    pub fn get_width(&self) -> i32 {
        (self.right - self.left).abs()
    }

    /// Get the height of the rectangle.
    #[inline]
    pub fn get_height(&self) -> i32 {
        (self.bottom - self.top).abs()
    }

    /// Check if the rectangle is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.get_width() <= 0 || self.get_height() <= 0
    }

    /// Compute the intersection of two rectangles.
    #[inline]
    pub fn intersect(&self, other: &Self) -> Option<Self> {
        let result = Self {
            left: self.left.max(other.left),
            top: self.top.max(other.top),
            right: self.right.min(other.right),
            bottom: self.bottom.min(other.bottom),
        };
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rectangle_new() {
        let r = Rectangle::new(1u32, 2, 10, 20);
        assert_eq!(r.left, 1);
        assert_eq!(r.top, 2);
        assert_eq!(r.right, 10);
        assert_eq!(r.bottom, 20);
    }

    #[test]
    fn test_rectangle_dimensions() {
        let r = Rectangle::<u32>::from_dimensions(100, 200);
        assert_eq!(r.get_width(), 100);
        assert_eq!(r.get_height(), 200);
    }

    #[test]
    fn test_rectangle_empty() {
        let r = Rectangle::new(0u32, 0, 0, 0);
        assert!(r.is_empty());
        let r = Rectangle::new(0u32, 0, 10, 10);
        assert!(!r.is_empty());
    }

    #[test]
    fn test_rectangle_intersect() {
        let a = Rectangle::new(0u32, 0, 10, 10);
        let b = Rectangle::new(5u32, 5, 15, 15);
        let c = a.intersect(&b).unwrap();
        assert_eq!(c, Rectangle::new(5, 5, 10, 10));
    }

    #[test]
    fn test_rectangle_no_intersect() {
        let a = Rectangle::new(0u32, 0, 5, 5);
        let b = Rectangle::new(10u32, 10, 15, 15);
        assert!(a.intersect(&b).is_none());
    }

    #[test]
    fn test_rectangle_translate() {
        let r = Rectangle::new(0u32, 0, 10, 10);
        let t = r.translate_x(5);
        assert_eq!(t, Rectangle::new(5, 0, 15, 10));
        let t = r.translate_y(3);
        assert_eq!(t, Rectangle::new(0, 3, 10, 13));
    }

    #[test]
    fn test_pi() {
        assert!((PI - std::f32::consts::PI).abs() < 1e-6);
    }
}

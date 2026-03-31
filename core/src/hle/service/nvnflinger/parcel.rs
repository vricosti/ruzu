// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/parcel.h

use common::alignment;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ParcelHeader {
    pub data_size: u32,
    pub data_offset: u32,
    pub objects_size: u32,
    pub objects_offset: u32,
}
const _: () = assert!(std::mem::size_of::<ParcelHeader>() == 16);

/// Parcel reader for incoming binder transaction data.
pub struct InputParcel<'a> {
    read_buffer: &'a [u8],
    read_index: usize,
}

impl<'a> InputParcel<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        let mut parcel = Self {
            read_buffer: data,
            read_index: 0,
        };
        parcel.deserialize_header();
        // Read and discard interface token (matches upstream constructor)
        let _token = parcel.read_interface_token();
        parcel
    }

    /// Read a value of type T from the parcel.
    /// T must be a plain-old-data type (Copy).
    pub fn read<T: Copy + Default>(&mut self) -> T {
        let size = std::mem::size_of::<T>();
        assert!(
            self.read_index + size <= self.read_buffer.len(),
            "InputParcel::read out of bounds"
        );
        let mut val = T::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.read_buffer.as_ptr().add(self.read_index),
                &mut val as *mut T as *mut u8,
                size,
            );
        }
        self.read_index += size;
        // Align to 4 bytes
        self.read_index = alignment::align_up(self.read_index as u64, 4) as usize;
        val
    }

    /// Read a flattened value: reads an s64 size first, then the data.
    pub fn read_flattened<T: Copy + Default>(&mut self) -> T {
        let flattened_size = self.read::<i64>();
        assert_eq!(
            std::mem::size_of::<T>() as i64,
            flattened_size,
            "InputParcel::read_flattened size mismatch"
        );
        self.read::<T>()
    }

    /// Read a value without alignment adjustment.
    pub fn read_unaligned<T: Copy + Default>(&mut self) -> T {
        let size = std::mem::size_of::<T>();
        assert!(
            self.read_index + size <= self.read_buffer.len(),
            "InputParcel::read_unaligned out of bounds"
        );
        let mut val = T::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.read_buffer.as_ptr().add(self.read_index),
                &mut val as *mut T as *mut u8,
                size,
            );
        }
        self.read_index += size;
        val
    }

    /// Read an optional flattened object. Returns None if the validity flag is false.
    pub fn read_object<T: Copy + Default>(&mut self) -> Option<T> {
        let is_valid = self.read::<u8>() != 0;
        if is_valid {
            Some(self.read_flattened::<T>())
        } else {
            None
        }
    }

    /// Read the interface token string (UTF-16).
    pub fn read_interface_token(&mut self) -> Vec<u16> {
        let _unknown = self.read::<u32>();
        let length = self.read::<u32>();
        let mut token = Vec::with_capacity(length as usize + 1);
        for _ in 0..(length + 1) {
            token.push(self.read_unaligned::<u16>());
        }
        self.read_index = alignment::align_up(self.read_index as u64, 4) as usize;
        token
    }

    fn deserialize_header(&mut self) {
        assert!(
            self.read_buffer.len() > std::mem::size_of::<ParcelHeader>(),
            "InputParcel: buffer too small for header"
        );
        let mut header = ParcelHeader::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.read_buffer.as_ptr(),
                &mut header as *mut ParcelHeader as *mut u8,
                std::mem::size_of::<ParcelHeader>(),
            );
        }
        self.read_index = header.data_offset as usize;
    }
}

/// Parcel writer for outgoing binder transaction data.
pub struct OutputParcel {
    data_buffer: Vec<u8>,
    object_buffer: Vec<u8>,
}

impl OutputParcel {
    pub fn new() -> Self {
        Self {
            data_buffer: Vec::new(),
            object_buffer: Vec::new(),
        }
    }

    /// Write a value to the data section.
    pub fn write<T: Copy>(&mut self, val: &T) {
        self.write_impl(val, false);
    }

    /// Write a flattened object to the data section.
    /// Writes a u32 validity flag, then i64 size, then the data.
    pub fn write_flattened_object<T: Copy>(&mut self, val: Option<&T>) {
        match val {
            None => {
                let zero: u32 = 0;
                self.write(&zero);
            }
            Some(v) => {
                let one: u32 = 1;
                self.write(&one);
                let size = std::mem::size_of::<T>() as i64;
                self.write(&size);
                self.write(v);
            }
        }
    }

    /// Write a value to data and a zero u32 to objects (for interface writes).
    pub fn write_interface<T: Copy>(&mut self, val: &T) {
        self.write_impl(val, false);
        let zero: u32 = 0;
        self.write_to_objects(&zero);
    }

    /// Serialize into a parcel byte buffer and return it.
    pub fn serialize(&mut self) -> Vec<u8> {
        let total_size =
            std::mem::size_of::<ParcelHeader>() + self.data_buffer.len() + self.object_buffer.len();
        let mut output = vec![0u8; total_size];

        let header = ParcelHeader {
            data_size: self.data_buffer.len() as u32,
            data_offset: std::mem::size_of::<ParcelHeader>() as u32,
            objects_size: self.object_buffer.len() as u32,
            objects_offset: (std::mem::size_of::<ParcelHeader>() + self.data_buffer.len()) as u32,
        };

        unsafe {
            std::ptr::copy_nonoverlapping(
                &header as *const ParcelHeader as *const u8,
                output.as_mut_ptr(),
                std::mem::size_of::<ParcelHeader>(),
            );
        }

        let data_start = header.data_offset as usize;
        output[data_start..data_start + self.data_buffer.len()].copy_from_slice(&self.data_buffer);

        let obj_start = header.objects_offset as usize;
        output[obj_start..obj_start + self.object_buffer.len()]
            .copy_from_slice(&self.object_buffer);

        output
    }

    fn write_impl<T: Copy>(&mut self, val: &T, _to_objects: bool) {
        let size = std::mem::size_of::<T>();
        let aligned_size = alignment::align_up(size as u64, 4) as usize;
        let old_len = self.data_buffer.len();
        self.data_buffer.resize(old_len + aligned_size, 0);
        unsafe {
            std::ptr::copy_nonoverlapping(
                val as *const T as *const u8,
                self.data_buffer.as_mut_ptr().add(old_len),
                size,
            );
        }
    }

    fn write_to_objects<T: Copy>(&mut self, val: &T) {
        let size = std::mem::size_of::<T>();
        let aligned_size = alignment::align_up(size as u64, 4) as usize;
        let old_len = self.object_buffer.len();
        self.object_buffer.resize(old_len + aligned_size, 0);
        unsafe {
            std::ptr::copy_nonoverlapping(
                val as *const T as *const u8,
                self.object_buffer.as_mut_ptr().add(old_len),
                size,
            );
        }
    }
}

impl Default for OutputParcel {
    fn default() -> Self {
        Self::new()
    }
}

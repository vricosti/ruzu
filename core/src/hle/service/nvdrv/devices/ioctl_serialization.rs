// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/ioctl_serialization.h
//!
//! This module provides ioctl serialization helpers that handle the marshalling
//! of fixed-size and variable-size arguments between raw byte buffers and typed
//! Rust structures.

use super::super::nvdata::NvResult;

/// Marker type used as a placeholder when no argument of that type exists.
pub struct Null;

/// Deserializes a fixed-size argument from the input buffer, calls the handler,
/// and writes the result back to the output buffer.
///
/// Port of WrapFixed.
pub fn wrap_fixed<T, F>(input: &[u8], output: &mut [u8], handler: F) -> NvResult
where
    T: Default + Copy,
    F: FnOnce(&mut T) -> NvResult,
{
    let mut fixed = T::default();
    let size = std::mem::size_of::<T>();
    let copy_in = size.min(input.len());
    if copy_in > 0 {
        // Safety: T is Copy and repr(C), we copy raw bytes
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr(),
                &mut fixed as *mut T as *mut u8,
                copy_in,
            );
        }
    }

    let result = handler(&mut fixed);

    let copy_out = size.min(output.len());
    if copy_out > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                &fixed as *const T as *const u8,
                output.as_mut_ptr(),
                copy_out,
            );
        }
    }

    result
}

/// Deserializes a variable-length array of elements from the input buffer,
/// calls the handler, and writes the result back to the output buffer.
///
/// Port of WrapVariable.
pub fn wrap_variable<T, F>(input: &[u8], output: &mut [u8], handler: F) -> NvResult
where
    T: Default + Copy,
    F: FnOnce(&mut Vec<T>) -> NvResult,
{
    let elem_size = std::mem::size_of::<T>();
    let num_elems = if elem_size > 0 {
        input.len() / elem_size
    } else {
        0
    };

    let mut var_args: Vec<T> = vec![T::default(); num_elems];
    if num_elems > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr(),
                var_args.as_mut_ptr() as *mut u8,
                num_elems * elem_size,
            );
        }
    }

    let result = handler(&mut var_args);

    if num_elems > 0 && output.len() >= num_elems * elem_size {
        unsafe {
            std::ptr::copy_nonoverlapping(
                var_args.as_ptr() as *const u8,
                output.as_mut_ptr(),
                num_elems * elem_size,
            );
        }
    }

    result
}

/// Deserializes a fixed-size argument and an inline output variable array,
/// calls the handler, and writes both back to the output and inline_output buffers.
///
/// Port of WrapFixedInlOut.
pub fn wrap_fixed_inl_out<T, U, F>(
    input: &[u8],
    output: &mut [u8],
    inline_output: &mut [u8],
    handler: F,
) -> NvResult
where
    T: Default + Copy,
    U: Default + Copy,
    F: FnOnce(&mut T, &mut Vec<U>) -> NvResult,
{
    let fixed_size = std::mem::size_of::<T>();
    let mut fixed = T::default();
    let copy_in = fixed_size.min(input.len());
    if copy_in > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr(),
                &mut fixed as *mut T as *mut u8,
                copy_in,
            );
        }
    }

    let elem_size = std::mem::size_of::<U>();
    let num_inl_out = if elem_size > 0 {
        inline_output.len() / elem_size
    } else {
        0
    };
    let mut inl_out_args: Vec<U> = vec![U::default(); num_inl_out];

    let result = handler(&mut fixed, &mut inl_out_args);

    let copy_out = fixed_size.min(output.len());
    if copy_out > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                &fixed as *const T as *const u8,
                output.as_mut_ptr(),
                copy_out,
            );
        }
    }

    if num_inl_out > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                inl_out_args.as_ptr() as *const u8,
                inline_output.as_mut_ptr(),
                num_inl_out * elem_size,
            );
        }
    }

    result
}

/// Deserializes a fixed-size argument followed by a variable array from the input buffer.
///
/// Port of WrapFixedVariable.
pub fn wrap_fixed_variable<T, U, F>(input: &[u8], output: &mut [u8], handler: F) -> NvResult
where
    T: Default + Copy,
    U: Default + Copy,
    F: FnOnce(&mut T, &mut Vec<U>) -> NvResult,
{
    let fixed_size = std::mem::size_of::<T>();
    let mut fixed = T::default();
    let var_offset = fixed_size.min(input.len());
    if var_offset > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr(),
                &mut fixed as *mut T as *mut u8,
                var_offset,
            );
        }
    }

    let elem_size = std::mem::size_of::<U>();
    let num_var = if elem_size > 0 && input.len() > var_offset {
        (input.len() - var_offset) / elem_size
    } else {
        0
    };
    let mut var_args: Vec<U> = vec![U::default(); num_var];
    if num_var > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr().add(var_offset),
                var_args.as_mut_ptr() as *mut u8,
                num_var * elem_size,
            );
        }
    }

    let result = handler(&mut fixed, &mut var_args);

    let copy_fixed = fixed_size.min(output.len());
    if copy_fixed > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                &fixed as *const T as *const u8,
                output.as_mut_ptr(),
                copy_fixed,
            );
        }
    }
    if num_var > 0 && output.len() > var_offset {
        let max_var = output.len() - var_offset;
        let copy_var = (num_var * elem_size).min(max_var);
        unsafe {
            std::ptr::copy_nonoverlapping(
                var_args.as_ptr() as *const u8,
                output.as_mut_ptr().add(var_offset),
                copy_var,
            );
        }
    }

    result
}

/// Deserializes a fixed-size argument and inline input variable array from the input
/// and inline_input buffers.
///
/// Port of WrapFixedInlIn.
pub fn wrap_fixed_inl_in<T, U, F>(
    input: &[u8],
    inline_input: &[u8],
    output: &mut [u8],
    handler: F,
) -> NvResult
where
    T: Default + Copy,
    U: Default + Copy,
    F: FnOnce(&mut T, &[U]) -> NvResult,
{
    let fixed_size = std::mem::size_of::<T>();
    let mut fixed = T::default();
    let copy_in = fixed_size.min(input.len());
    if copy_in > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr(),
                &mut fixed as *mut T as *mut u8,
                copy_in,
            );
        }
    }

    let elem_size = std::mem::size_of::<U>();
    let num_inl_in = if elem_size > 0 {
        inline_input.len() / elem_size
    } else {
        0
    };
    let mut inl_in_args: Vec<U> = vec![U::default(); num_inl_in];
    if num_inl_in > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                inline_input.as_ptr(),
                inl_in_args.as_mut_ptr() as *mut u8,
                num_inl_in * elem_size,
            );
        }
    }

    let result = handler(&mut fixed, &inl_in_args);

    let copy_out = fixed_size.min(output.len());
    if copy_out > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(
                &fixed as *const T as *const u8,
                output.as_mut_ptr(),
                copy_out,
            );
        }
    }

    result
}

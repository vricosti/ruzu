pub(crate) fn write_copy<T: Copy>(value: &T, output: &mut [u8]) -> usize {
    let size = std::mem::size_of::<T>();
    if output.len() < size {
        return 0;
    }
    unsafe {
        std::ptr::copy_nonoverlapping(value as *const T as *const u8, output.as_mut_ptr(), size);
    }
    size
}

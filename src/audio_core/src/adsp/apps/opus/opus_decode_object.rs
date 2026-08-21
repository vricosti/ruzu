use std::ffi::{c_int, c_void};
use std::mem::size_of;

pub const DECODE_OBJECT_MAGIC: u32 = 0xDEAD_BEEF;

pub(crate) const OPUS_OK: c_int = 0;
pub(crate) const OPUS_INVALID_PACKET: c_int = -4;
pub(crate) const OPUS_INVALID_STATE: c_int = -6;
const OPUS_RESET_STATE: c_int = 4028;
const OPUS_GET_FINAL_RANGE_REQUEST: c_int = 4031;
const OPUS_DECODE_OBJECT_SIZE: u32 = 0x20;

// Linkage against libopus is declared by `src/audio_core/build.rs`, which probes
// pkg-config so the linker also learns where the library lives.
unsafe extern "C" {
    fn opus_decoder_get_size(channels: c_int) -> c_int;
    fn opus_decoder_init(st: *mut c_void, sample_rate: c_int, channels: c_int) -> c_int;
    fn opus_decode(
        st: *mut c_void,
        data: *const u8,
        len: c_int,
        pcm: *mut i16,
        frame_size: c_int,
        decode_fec: c_int,
    ) -> c_int;
    fn opus_decoder_ctl(st: *mut c_void, request: c_int, ...) -> c_int;
}

pub struct OpusDecodeObject {
    magic: u32,
    initialized: bool,
    state_valid: bool,
    self_buffer: u64,
    final_range: u32,
    decoder_storage: Vec<usize>,
}

impl OpusDecodeObject {
    fn is_valid_channel_count(channel_count: u32) -> bool {
        matches!(channel_count, 1 | 2)
    }

    pub fn get_work_buffer_size(channel_count: u32) -> u32 {
        if !Self::is_valid_channel_count(channel_count) {
            return 0;
        }
        let decoder_size = unsafe { opus_decoder_get_size(channel_count as c_int) };
        if decoder_size <= 0 {
            return 0;
        }
        OPUS_DECODE_OBJECT_SIZE + decoder_size as u32
    }

    pub fn initialize(buffer: u64, comparison_buffer: u64, existing: Option<Self>) -> Self {
        match existing {
            Some(mut decode_object) => {
                if decode_object.magic == DECODE_OBJECT_MAGIC {
                    if !decode_object.initialized || decode_object.self_buffer == comparison_buffer
                    {
                        decode_object.state_valid = true;
                    }
                } else {
                    decode_object.magic = 0;
                    decode_object.initialized = false;
                    decode_object.state_valid = true;
                    decode_object.self_buffer = buffer;
                    decode_object.final_range = 0;
                }
                if decode_object.self_buffer == 0 {
                    decode_object.self_buffer = buffer;
                }
                decode_object
            }
            None => Self {
                magic: 0,
                initialized: false,
                state_valid: true,
                self_buffer: buffer,
                final_range: 0,
                decoder_storage: Vec::new(),
            },
        }
    }

    pub fn initialize_decoder(&mut self, sample_rate: u32, channel_count: u32) -> c_int {
        if !self.state_valid {
            return OPUS_INVALID_STATE;
        }
        if self.initialized {
            return OPUS_OK;
        }
        let decoder_size = unsafe { opus_decoder_get_size(channel_count as c_int) };
        if decoder_size <= 0 {
            return -1;
        }
        let word_size = size_of::<usize>();
        self.decoder_storage = vec![0; (decoder_size as usize).div_ceil(word_size)];
        let result = unsafe {
            opus_decoder_init(
                self.decoder_storage.as_mut_ptr().cast(),
                sample_rate as c_int,
                channel_count as c_int,
            )
        };
        if result == OPUS_OK {
            self.magic = DECODE_OBJECT_MAGIC;
            self.initialized = true;
            self.state_valid = true;
            self.final_range = 0;
        }
        result
    }

    pub fn shutdown(&mut self) -> c_int {
        if !self.state_valid {
            return OPUS_INVALID_STATE;
        }
        if self.initialized {
            self.magic = 0;
            self.initialized = false;
            self.state_valid = false;
            self.self_buffer = 0;
            self.final_range = 0;
            self.decoder_storage.clear();
        }
        OPUS_OK
    }

    pub fn reset_decoder(&mut self) -> c_int {
        if !self.state_valid || !self.initialized {
            return OPUS_INVALID_STATE;
        }
        let result =
            unsafe { opus_decoder_ctl(self.decoder_storage.as_mut_ptr().cast(), OPUS_RESET_STATE) };
        result
    }

    pub fn decode(
        &mut self,
        out_sample_count: &mut u32,
        output_data: &mut [u8],
        input_data: &[u8],
    ) -> c_int {
        if !self.state_valid || !self.initialized {
            return OPUS_INVALID_STATE;
        }
        *out_sample_count = 0;
        if self.decoder_storage.is_empty() {
            return OPUS_INVALID_STATE;
        }

        let decoded = unsafe {
            opus_decode(
                self.decoder_storage.as_mut_ptr().cast(),
                input_data.as_ptr(),
                input_data.len() as c_int,
                output_data.as_mut_ptr().cast(),
                output_data.len() as c_int,
                0,
            )
        };
        if decoded < OPUS_OK {
            return decoded;
        }

        *out_sample_count = decoded as u32;
        let mut final_range = 0u32;
        let result = unsafe {
            opus_decoder_ctl(
                self.decoder_storage.as_mut_ptr().cast(),
                OPUS_GET_FINAL_RANGE_REQUEST,
                &mut final_range as *mut u32,
            )
        };
        self.final_range = final_range;
        result
    }

    pub fn get_final_range(&self) -> u32 {
        self.final_range
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    const OPUS_APPLICATION_AUDIO: c_int = 2049;

    unsafe extern "C" {
        fn opus_encoder_create(
            sample_rate: c_int,
            channels: c_int,
            application: c_int,
            error: *mut c_int,
        ) -> *mut c_void;
        fn opus_encode(
            st: *mut c_void,
            pcm: *const i16,
            frame_size: c_int,
            data: *mut u8,
            max_data_bytes: c_int,
        ) -> c_int;
        fn opus_encoder_destroy(st: *mut c_void);
    }

    pub(crate) fn encoded_stereo_packet() -> Vec<u8> {
        let mut error = 0;
        let encoder = unsafe { opus_encoder_create(48_000, 2, OPUS_APPLICATION_AUDIO, &mut error) };
        assert_eq!(error, OPUS_OK);
        assert!(!encoder.is_null());

        let mut pcm = vec![0i16; 960 * 2];
        for frame in 0..960 {
            let sample = (((frame % 96) as i16) - 48) * 400;
            pcm[frame * 2] = sample;
            pcm[frame * 2 + 1] = sample;
        }
        let mut packet = vec![0u8; 4_000];
        let packet_size = unsafe {
            opus_encode(
                encoder,
                pcm.as_ptr(),
                960,
                packet.as_mut_ptr(),
                packet.len() as c_int,
            )
        };
        unsafe { opus_encoder_destroy(encoder) };
        assert!(packet_size > 0);
        packet.truncate(packet_size as usize);
        packet
    }

    #[test]
    fn decodes_non_silent_pcm_with_libopus() {
        let mut object = OpusDecodeObject::initialize(0x1000, 0x1000, None);
        assert_eq!(object.initialize_decoder(48_000, 2), OPUS_OK);

        let packet = encoded_stereo_packet();
        let mut output = vec![0u8; 960 * 2 * size_of::<i16>()];
        let mut sample_count = 0;
        assert_eq!(
            object.decode(&mut sample_count, &mut output, &packet),
            OPUS_OK
        );
        assert_eq!(sample_count, 960);
        assert!(output.chunks_exact(2).any(|bytes| bytes != [0, 0]));
        assert_ne!(object.get_final_range(), 0);
    }
}

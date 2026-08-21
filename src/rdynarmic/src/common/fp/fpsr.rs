#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Fpsr {
    value: u32,
}

impl Fpsr {
    const MASK: u32 = 0xf800_009f;

    pub const fn new(data: u32) -> Self {
        Self {
            value: data & Self::MASK,
        }
    }

    pub const fn value(self) -> u32 {
        self.value
    }

    pub fn set_n(&mut self, value: bool) {
        self.set_bit(31, value);
    }

    pub fn set_z(&mut self, value: bool) {
        self.set_bit(30, value);
    }

    pub fn set_c(&mut self, value: bool) {
        self.set_bit(29, value);
    }

    pub fn set_v(&mut self, value: bool) {
        self.set_bit(28, value);
    }

    pub fn set_qc(&mut self, value: bool) {
        self.set_bit(27, value);
    }

    pub fn set_idc(&mut self, value: bool) {
        self.set_bit(7, value);
    }

    pub fn set_ixc(&mut self, value: bool) {
        self.set_bit(4, value);
    }

    pub fn set_ufc(&mut self, value: bool) {
        self.set_bit(3, value);
    }

    pub fn set_ofc(&mut self, value: bool) {
        self.set_bit(2, value);
    }

    pub fn set_dzc(&mut self, value: bool) {
        self.set_bit(1, value);
    }

    pub fn set_ioc(&mut self, value: bool) {
        self.set_bit(0, value);
    }

    fn set_bit(&mut self, bit: u32, value: bool) {
        self.value = (self.value & !(1 << bit)) | ((value as u32) << bit);
    }
}

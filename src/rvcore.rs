use std::rc::Rc;

use crate::rv32i::RefMod;

pub struct EEI {
    // main memory, or I/O
    pub mem: Rc<MemMod>,
    pub core: rv64i::RefMod,
}

impl EEI {
    pub fn new() -> Self {
        let mem = Rc::new(MemMod::new());
        Self {
            mem: mem.clone(),
            core: RefMod::new(mem),
        }
    }

    pub fn exec(&mut self) {

    }
}

pub struct MemMod {
    pub data: Vec<u8>
}

impl MemMod {
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(4096)
        }
    }
}

pub trait MemRW {
    fn read_u8(&self, addr: usize) -> u8;
    fn read_u16(&self, addr: usize) -> u16;
    fn read_u32(&self, addr: usize) -> u32;

    fn write_u8(&mut self, addr: usize, val: u8);
    fn write_u16(&mut self, addr: usize, val: u16);
    fn write_u32(&mut self, addr: usize, val: u32);
}

// assume little endian
// maybe add perm checks?
// handling misalignment of addresses is on EEI
impl MemRW for MemMod {
    fn read_u8(&self, addr: usize) -> u8 {
        self.data[addr]
    }

    fn read_u16(&self, addr: usize) -> u16 {
        if addr & 0b1 != 0 {
            // misaligned loads and stores
            todo!()
        }

        (self.data[addr] as u16) +
        (self.data[addr + 1] as u16) << 8
    }

    fn read_u32(&self, addr: usize) -> u32 {
        if addr & 0b11 != 0 {
            // misaligned loads and stores
            todo!()
        }

        (self.data[addr] as u32) +
        (self.data[addr + 1] as u32) << 8 +
        (self.data[addr + 1] as u32) << 16 +
        (self.data[addr + 1] as u32) << 24
    }

    fn write_u8(&mut self, addr: usize, val: u8) {
        self.data[addr] = val;
    }

    fn write_u16(&mut self, addr: usize, val: u16) {
        if addr & 0b1 != 0 {
            // misaligned loads and stores
            todo!()
        }

        self.data[addr] = (val & 0xff) as u8;
        self.data[addr + 1] = (val & 0xff00) as u8;
    }

    fn write_u32(&mut self, addr: usize, val: u32) {
        if addr & 0b11 != 0 {
            // misaligned loads and stores
            todo!()
        }

        self.data[addr] = (val & 0xff) as u8;
        self.data[addr + 1] = (val & (0xff << 8)) as u8;
        self.data[addr + 2] = (val & (0xff << 16)) as u8;
        self.data[addr + 3] = (val & (0xff << 24)) as u8;
    }
}

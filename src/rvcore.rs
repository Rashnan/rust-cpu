use std::{cell::RefCell, rc::Rc, ops::Range};

use crate::rv32i::RefMod;

const MAX_COMPUTE: usize = 100;

pub struct Eei {
    // main memory, or I/O
    pub mem: Rc<RefCell<MemMod>>,
    pub core: RefMod,
}

impl Eei {
    pub fn new() -> Self {
        let mem = Rc::new(RefCell::new(MemMod::new()));
        Self {
            mem: mem.clone(),
            core: RefMod::new(mem),
        }
    }

    pub fn run(&mut self) {
        let mut i = 0;
        while self.core.pc != u32::MAX && i < MAX_COMPUTE {
            self.exec(i);
            i += 1;
        }
    }

    pub fn run_range(&mut self, rng: Range<u32>) {
        let mut i = 0;
        while self.core.pc >= rng.start && self.core.pc < rng.end && i < MAX_COMPUTE {
            self.exec(i);
            i += 1;
        }
    }

    pub fn exec(&mut self, count: usize) {
        let inst = self.mem.borrow().read_u32(self.core.pc as usize);
        println!("{:03}. {:02} -- {:09}", count, self.core.pc, inst);
        
        self.core.pc += 4;
        if !self.core.trap(inst) {
            // check other extensions
            todo!()
        }
    }
}

pub trait EeiCore {
    fn new(mem: Rc<RefCell<dyn MemRW>>) -> Self;
    fn trap(&mut self, inst: u32) -> bool;
}

pub struct MemMod {
    pub data: Vec<u8>
}

impl MemMod {
    pub fn new() -> Self {
        Self {
            data: vec![0; 4096]
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
// handling misalignment of addresses is on Eei
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
        ((self.data[addr + 1] as u16) << 8)
    }

    fn read_u32(&self, addr: usize) -> u32 {
        if addr & 0b11 != 0 {
            // misaligned loads and stores
            todo!()
        }

        (self.data[addr] as u32) +
        ((self.data[addr + 1] as u32) << 8) +
        ((self.data[addr + 2] as u32) << 16) +
        ((self.data[addr + 3] as u32) << 24)
    }

    fn write_u8(&mut self, addr: usize, val: u8) {
        self.data[addr] = val;
    }

    fn write_u16(&mut self, addr: usize, val: u16) {
        if addr & 0b1 != 0 {
            // misaligned loads and stores
            todo!()
        }

        self.data[addr] = val as u8;
        self.data[addr + 1] = (val >> 8) as u8;
    }

    fn write_u32(&mut self, addr: usize, val: u32) {
        if addr & 0b11 != 0 {
            // misaligned loads and stores
            todo!()
        }

        self.data[addr] = val as u8;
        self.data[addr + 1] = (val >> 8) as u8;
        self.data[addr + 2] = (val >> 16) as u8;
        self.data[addr + 3] = (val >> 24) as u8;
    }
}
// std exts
// M -- multiply & divide
// A  -- atomic
// F -- float
// D -- double float
// C -- 16b compressed within 32b
// V -- vector (parallel data ... same operations on many numbers)
// B -- bit

// Other: Q, L, J, T, P, N

// previlege levels:
// M -- machine
// S -- supervisor 
// U -- user

#[macro_use] extern crate enum_primitive;

use std::sync::{Arc, Mutex};

use bit::BitIndex;
use enum_primitive::{enum_from_primitive, FromPrimitive};

// major opcodes list
enum_from_primitive!{
    // R -- rd, rs1, rs2, funct10, opcode
    // R4 -- rd, rs1, rs2, rs3, funct5, opcode
    // I -- rd, rs1, imm[11:7], imm[6:0], funct3, opcode
    // B -- imm[11:7], rs1, rs2, imm[6:0], unct3, opcode
    // L -- rd, LUI imm[19:0], opcode
    // J -- offset[24:0], opcode

    #[derive(Debug, PartialEq)]
    pub enum MajorOpcodes {
        J = 0b11001,
        Jal = 0b11011,
        Jalr = 0b11010,
        Branch = 0b11000,
        Load = 0b00000,
        LoadFp = 0b00001,
        Store = 0b01000,
        StoreFp = 0b01001,
        Amo = 0b01010,
        OpImm = 0b00100,
        OpImm32 = 0b00110,
        Op = 0b01100,
        Op32 = 0b01110,
        OpFp = 0b10100,

        MiscMem = 0b01011,
        Lui = 0b01101,
        MAdd = 0b10000,
        MSub = 0b10001,
        NMSub = 0b10010,
        NMAdd = 0b10011,
        System = 0b11101
    }
}

// instruction types
pub struct InstR {
    pub rd: usize,
    pub rs1: usize,
    pub rs2: usize,
    pub funct10: u32,
    pub opcode: u32,
}

pub fn extract_r(inst: u32) -> InstR {
    InstR {
        rd: inst.bit_range(27..32) as usize,
        rs1: inst.bit_range(22..27) as usize,
        rs2: inst.bit_range(17..22) as usize,
        funct10: inst.bit_range(7..17),
        opcode: inst.bit_range(0..7)
    }
}

pub struct InstR4 {
    pub rd: usize,
    pub rs1: usize,
    pub rs2: usize,
    pub rs3: usize,
    pub funct5: u32,
    pub opcode: u32,
}

pub fn extract_r4(inst: u32) -> InstR4 {
    InstR4 {
        rd: inst.bit_range(27..32) as usize,
        rs1: inst.bit_range(22..27) as usize,
        rs2: inst.bit_range(17..22) as usize,
        rs3: inst.bit_range(12..17) as usize,
        funct5: inst.bit_range(7..12),
        opcode: inst.bit_range(0..7)
    }
}

pub struct InstI {
    pub rd: usize,
    pub rs1: usize,
    pub imm_11_7: u32,
    pub imm_6_0: u32,
    pub funct3: u32,
    pub opcode: u32,
}

pub fn extract_i(inst: u32) -> InstI {
    InstI {
        rd: inst.bit_range(27..32) as usize,
        rs1: inst.bit_range(22..27) as usize,
        imm_11_7: inst.bit_range(17..22),
        imm_6_0: inst.bit_range(10..17),
        funct3: inst.bit_range(7..10),
        opcode: inst.bit_range(0..7)
    }
}

pub struct InstB {
    pub imm_11_7: u32,
    pub rs1: usize,
    pub rs2: usize,
    pub imm_6_0: u32,
    pub funct3: u32,
    pub opcode: u32,
}

pub fn extract_b(inst: u32) -> InstB {
    InstB {
        imm_11_7: inst.bit_range(27..32),
        rs1: inst.bit_range(22..27) as usize,
        rs2: inst.bit_range(17..22) as usize,
        imm_6_0: inst.bit_range(10..17),
        funct3: inst.bit_range(7..10),
        opcode: inst.bit_range(0..7)
    }
}

pub struct InstL {
    pub rd: usize,
    pub lui20: u32,
    pub opcode: u32,
}

pub fn extract_l(inst: u32) -> InstL {
    InstL { 
        rd: inst.bit_range(27..32) as usize,
        lui20: inst.bit_range(7..27),
        opcode: inst.bit_range(0..7)
    }
}

pub struct InstJ {
    pub off25: u32,
    pub opcode: u32,
}

pub fn extract_j(inst: u32) -> InstJ {
    InstJ { 
        off25: inst.bit_range(7..32),
        opcode: inst.bit_range(0..7)
    }
}

// module traits

pub mod modi64 {
    use std::sync::{Arc, Mutex};

    use enum_primitive::FromPrimitive;

    use crate::*;

    // all jalr instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstJALR {
            C,
            R,
            J,
            RDNPC = 0b100
        }
    }

    // all load type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstLoad {
            LB = 0x0,
            LH = 0b1,
            LW = 0b10,
            LD = 0b11,
            LBU = 0b100,
            LHU = 0b101,
            LWU = 0b110,
        }
    }

    // all store type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstStore {
            SB,
            SH,
            SW,
            SD,
        }
    }

    // all op-imm type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstOpImm {
            ADDI,
            SLLI,
            SLTI,
            SLTIU,
            XORI,
            SRxI,
            ORI,
            ANDI,
        }
    }

    // all op-imm-32 type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstOpImm32 {
            ADDIW,
            SLLIW,
            SRxIW = 0b101,
        }
    }

    // all op type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstOp {
            ADDx,
            SLL,
            SLT,
            SLTU,
            XOR,
            SRx,
            OR,
            AND,
        }
    }

    // all op-32 type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstOp32 {
            ADDx,
            SLL,
            SRx,
        }
    }

    pub struct Module {
        // referred to as xN where N is the number
        // x0 == 0
        // x1 - x31 are general purpose

        // x1 -- ra -- return address
        // x2 -- sp -- stack pointer
        // x3 -- gp -- global pointer
        // x4 -- tp -- thread local storage
        // x5 - x11 or t0 - t6 -- temp calc vals
        // x10 - x17 or a0 - a7 -- fn arg regs
        // x8, x9, x18 - x27 or s0 - s11 -- saved regs for preserving vals across fn calls
        // x28 - x31 or t3 - t6 -- additional temp regs
        pub regs: [u64; 32],
        pub pc: u64,

        // user memory
        pub mem: Arc<Mutex<Vec<u64>>>,
    }

    impl Module {
        pub fn new(mem: Arc<Mutex<Vec<u64>>>) -> Self {
            Self {
                regs: [0; 32],
                pc: 0,
                mem
            }
        }

        pub fn jump(&mut self, inst: u32) {
            let InstJ { off25, opcode: _ } = extract_j(inst);

            self.pc = (self.pc as i128 + ((off25 as i64) << 1) as i128) as u64;
        }

        pub fn jal(&mut self, inst: u32) {
            let InstJ { off25, opcode: _ } = extract_j(inst);

            self.regs[1] = self.pc + 4;
            self.pc = (self.pc as i128 + ((off25 as i64) << 1) as i128) as u64;
        }

        pub fn jalr(&mut self, inst: u32) {
            let InstI { rd, rs1, imm_11_7, imm_6_0, funct3, opcode: _ } = extract_i(inst);

            let imm12 = (imm_11_7 << 6 + imm_6_0) as u64;
            let addr = self.regs[rs1] as u64 + imm12;

            match InstJALR::from_u32(funct3).unwrap() {
                InstJALR::C | InstJALR::R | InstJALR::J => {
                    // C -- call subroutines
                    // R - return from subroutines
                    // J -- indirect jumps
                    // virtually all the same

                    if rd != 0 {
                        // x0 will ignore ret dest
                        self.regs[rd] = self.pc + 4;
                    }
                    self.pc = addr;
                },
                InstJALR::RDNPC => {
                    if rd != 0 {
                        self.regs[rd] = self.pc + 4;
                    }
                },
            }
        }

        pub fn branch(&mut self, inst: u32) {

        }
        

        pub fn load(&mut self, inst: u32) {
            let InstI { rd, rs1, imm_11_7, imm_6_0, funct3, opcode: _ } = extract_i(inst);

            if rd == 0 {
                return;
            }

            let imm12 = (imm_11_7 << 6 + imm_6_0) as usize;
            let addr = self.regs[rs1] as usize + imm12;

            let mem = self.mem.lock().unwrap();
            
            match InstLoad::from_u32(funct3).unwrap() {
                InstLoad::LB => self.regs[rd] = mem[addr] as i8 as u64,
                InstLoad::LH => self.regs[rd] = mem[addr] as i16 as u64,
                InstLoad::LW => self.regs[rd] = mem[addr] as i32 as u64,
                InstLoad::LD => self.regs[rd] = mem[addr] as i64 as u64,

                InstLoad::LBU => self.regs[rd] = mem[addr] as u8 as u64,
                InstLoad::LHU => self.regs[rd] = mem[addr] as u16 as u64,
                InstLoad::LWU => self.regs[rd] = mem[addr] as u32 as u64,
            }
        }

        pub fn store(&mut self, inst: u32) {
            let InstB { imm_11_7, rs1, rs2, imm_6_0, funct3, opcode: _ } = extract_b(inst);

            let imm12 = (imm_11_7 << 6 + imm_6_0) as usize;
            let addr = self.regs[rs1] as usize + imm12;

            let mut mem = self.mem.lock().unwrap();

            match InstStore::from_u32(funct3).unwrap() {
                InstStore::SB => mem[addr] &= self.regs[rs2] as i8 as u64,
                InstStore::SH => mem[addr] &= self.regs[rs2] as i16 as u64,
                InstStore::SW => mem[addr] &= self.regs[rs2] as i32 as u64,
                InstStore::SD => mem[addr] &= self.regs[rs2] as i64 as u64,
            }
        }
    

        pub fn op_imm(&mut self, inst: u32) {
            let InstI { rd, rs1, imm_11_7, imm_6_0, funct3, opcode: _ } = extract_i(inst);

            if rd == 0 {
                return;
            }

            let imm12 = (imm_11_7 << 6 + imm_6_0) as u64;

            match InstOpImm::from_u32(funct3).unwrap() {
                InstOpImm::ADDI => self.regs[rd] = self.regs[rs1] + imm12,
                
                InstOpImm::SLTI => self.regs[rd] = if (self.regs[rs1] as i64) < (imm12 as i64) { 1 } else { 0 },
                InstOpImm::SLTIU => self.regs[rd] = if self.regs[rs1] < imm12 { 1 } else { 0 },

                InstOpImm::XORI => self.regs[rd] = self.regs[rs1] ^ imm12,
                InstOpImm::ORI => self.regs[rd] = self.regs[rs1] | imm12,
                InstOpImm::ANDI => self.regs[rd] = self.regs[rs1] & imm12,

                InstOpImm::SLLI => self.regs[rd] = self.regs[rs1] << imm12.bit_range(0..6),
                InstOpImm::SRxI => {
                    // bit 0 to 5 used as shift (max shift = 2 ^ (5+1) - 1 ~ 61)
                    if imm12.bit(6) {
                        // arithmetic
                        self.regs[rd] = (self.regs[rs1] as i64 >> imm12.bit_range(0..6)) as u64
                    }
                    else {
                        // logical
                        self.regs[rd] = self.regs[rs1] as u64 >> imm12.bit_range(0..6)
                    }
                },
            }
        }

        pub fn op_imm32(&mut self, inst: u32) {
            let InstI { rd, rs1, imm_11_7, imm_6_0, funct3, opcode: _ } = extract_i(inst);

            if rd == 0 {
                return;
            }

            let imm12 = (imm_11_7 << 6 + imm_6_0) as u32;

            match InstOpImm32::from_u32(funct3).unwrap() {
                InstOpImm32::ADDIW => self.regs[rd] = (self.regs[rs1] as u32 + imm12) as u64,
                InstOpImm32::SLLIW => self.regs[rd] = ((self.regs[rs1] as u32) << imm12.bit_range(0..6)) as u64,
                InstOpImm32::SRxIW => {
                    // bit 0 to 5 used as shift (max shift = 2 ^ (5+1) - 1 ~ 61)
                    if imm12.bit(6) {
                        // arithmetic
                        self.regs[rd] = (self.regs[rs1] as i32 >> imm12.bit_range(0..6)) as u64
                    }
                    else {
                        // logical
                        self.regs[rd] = (self.regs[rs1] as u32 >> imm12.bit_range(0..6)) as u64
                    }
                },
            }
        }

        pub fn lui(&mut self, inst: u32) {
            let InstL { rd, lui20, opcode: _ } = extract_l(inst);

            if rd == 0 {
                return;
            }

            self.regs[rd] |= (lui20 << 19) as u64;
        }

        pub fn op(&mut self, inst: u32) {
            let InstR { rd, rs1, rs2, funct10, opcode: _ } = extract_r(inst);

            if rd == 0 {
                return;
            }

            match InstOp::from_u32(funct10.bit_range(0..3)).unwrap() {
                InstOp::ADDx => {
                    if funct10.bit(10) {
                        self.regs[rd] = self.regs[rs1] - self.regs[rs2]
                    }
                    else {
                        self.regs[rd] = self.regs[rs1] + self.regs[rs2]
                    }
                },
                InstOp::SLL => self.regs[rd] = self.regs[rs1] << self.regs[rs2].bit_range(0..6),
                InstOp::SLT => self.regs[rd] = if (self.regs[rs1] as i64) < (self.regs[rs2] as i64) { 1 } else { 0 },
                InstOp::SLTU => self.regs[rd] = if self.regs[rs1] < self.regs[rs2] { 1 } else { 0 },
                InstOp::XOR => self.regs[rd] = self.regs[rs1] ^ self.regs[rs2],
                InstOp::SRx => {
                    if funct10.bit(10) {
                        // arithmetic
                        self.regs[rd] = ((self.regs[rs1] as i64) >> (self.regs[rs2].bit_range(0..6) as i64)) as u64
                    }
                    else {
                        // logical
                        self.regs[rd] = self.regs[rs1] >> self.regs[rs2].bit_range(0..6)
                    }
                },
                InstOp::OR => self.regs[rd] = self.regs[rs1] | self.regs[rs2],
                InstOp::AND => self.regs[rd] = self.regs[rs1] & self.regs[rs2],
            }
        }

        pub fn op32(&mut self, inst: u32) {
            let InstR { rd, rs1, rs2, funct10, opcode: _ } = extract_r(inst);

            if rd == 0 {
                return;
            }

            match InstOp32::from_u32(funct10.bit_range(0..3)).unwrap() {
                InstOp32::ADDx => {
                    if funct10.bit(10) {
                        self.regs[rd] = (self.regs[rs1] as i32 - self.regs[rs2] as i32) as u64
                    }
                    else {
                        self.regs[rd] = (self.regs[rs1] as i32 + self.regs[rs2] as i32) as u64
                    }
                },
                InstOp32::SLL => self.regs[rd] = self.regs[rs1] << self.regs[rs2].bit_range(0..5),
                InstOp32::SRx => {
                    if funct10.bit(10) {
                        // arithmetic
                        self.regs[rd] = ((self.regs[rs1] as i64) >> (self.regs[rs2].bit_range(0..5) as i64)) as u64
                    }
                    else {
                        // logical
                        self.regs[rd] = self.regs[rs1] >> self.regs[rs2].bit_range(0..5)
                    }
                },
            }
        }
    }
}

pub mod moda {
    // todo!!

    pub struct Module {

    }
}

pub struct Machine {
    pub modi: Option<Box<modi64::Module>>,
    pub moda: Option<Box<moda::Module>>,

    pub ram: Arc<Mutex<Vec<u64>>>,
    pub mem: Arc<Mutex<Vec<u64>>>,
}

impl Machine {
    pub fn new() -> Self {
        let ram = Arc::new(Mutex::new(Vec::with_capacity(256)));
        let mem = Arc::new(Mutex::new(Vec::with_capacity(1024)));

        Self {
            modi: Some(Box::new(modi64::Module::new(mem.clone()))),
            moda: None,
            ram,
            mem,
        }
    }

    pub fn process(&mut self, inst: u32) {
        let opcode = inst.bit_range(0..7);

        // ensure we are only using 32b instructions
        const COMPRESSED: u32 = 1 << 2;
        const ABOVE32B: u32 = 1 << (2 + 3);
        if opcode.bit_range(0..2) & COMPRESSED != COMPRESSED {
            // may not complete
            todo!("16b compresssed instructions...");
        }

        if opcode.bit_range(0..6) & ABOVE32B != ABOVE32B {
            todo!("32b+ instructions...");
        }
        
        match MajorOpcodes::from_u32(opcode).unwrap() {
            // control transfer
            MajorOpcodes::J => todo!(),
            MajorOpcodes::Jal => todo!(),
            MajorOpcodes::Jalr => todo!(),
            MajorOpcodes::Branch => todo!(),
            
            // load
            MajorOpcodes::Load => {
                if let Some(modi) = &mut self.modi {
                    modi.load(inst);
                }
            },
            MajorOpcodes::LoadFp => unimplemented!(),

            // store
            MajorOpcodes::Store => {
                if let Some(modi) = &mut self.modi {
                    modi.store(inst);
                }
            },
            MajorOpcodes::StoreFp => unimplemented!(),

            // atomic memory operations
            MajorOpcodes::Amo => todo!(),

            MajorOpcodes::OpImm => {
                if let Some(modi) = &mut self.modi {
                    modi.op_imm(inst);
                }
            },
            MajorOpcodes::OpImm32 => {
                if let Some(modi) = &mut self.modi {
                    modi.op_imm32(inst);
                }
            },
            MajorOpcodes::Lui => {
                if let Some(modi) = &mut self.modi {
                    modi.lui(inst);
                }
            },
            MajorOpcodes::Op => {
                if let Some(modi) = &mut self.modi {
                    modi.op(inst);
                }
            },
            MajorOpcodes::Op32 => {
                if let Some(modi) = &mut self.modi {
                    modi.op32(inst);
                }
            },
            
            MajorOpcodes::MiscMem => todo!(),
            MajorOpcodes::MAdd => todo!(),
            MajorOpcodes::MSub => todo!(),
            MajorOpcodes::NMSub => todo!(),
            MajorOpcodes::NMAdd => todo!(),
            MajorOpcodes::OpFp => todo!(),
            MajorOpcodes::System => todo!(),
        }
        
        // match major_opcode {
        //     MajorOpcodes::Load => todo!(),
        //     MajorOpcodes::LoadFp => todo!(),
        //     MajorOpcodes::OpImm => todo!(),
        //     MajorOpcodes::OpImm32 => todo!(),
        //     MajorOpcodes::Store => todo!(),
        //     MajorOpcodes::StoreFp => todo!(),
        //     MajorOpcodes::Amo => todo!(),
        //     MajorOpcodes::MiscMem => todo!(),
        //     MajorOpcodes::Op => todo!(),
        //     MajorOpcodes::Lui => todo!(),
        //     MajorOpcodes::Op32 => todo!(),
        //     MajorOpcodes::MAdd => todo!(),
        //     MajorOpcodes::MSub => todo!(),
        //     MajorOpcodes::NMSub => todo!(),
        //     MajorOpcodes::NMAdd => todo!(),
        //     MajorOpcodes::OpFp => todo!(),
        //     MajorOpcodes::Branch => todo!(),
        //     MajorOpcodes::J => todo!(),
        //     MajorOpcodes::Jalr => todo!(),
        //     MajorOpcodes::Jal => todo!(),
        //     MajorOpcodes::System => todo!(),
            
        //     // _ => panic!("Unknowwn Opcode")
        // }
    }
}

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
        Load = 0b00000,// I
        LoadFp = 0b00001,// I
        OpImm = 0b00100,
        OpImm32 = 0b00110,
        Store = 0b01000,// B
        StoreFp = 0b01001,// B
        Amo = 0b01010,
        MiscMem = 0b01011,
        Op = 0b01100,
        Lui = 0b01101,
        Op32 = 0b01110,
        MAdd = 0b10000,
        MSub = 0b10001,
        NMSub = 0b10010,
        NMAdd = 0b10011,
        OpFp = 0b10100,
        Branch = 0b11000,
        J = 0b11001,
        Jalr = 0b11010,
        Jal = 0b11011,
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

pub mod rv128i {
    use bit::BitIndex;
    use enum_primitive::FromPrimitive;

    use crate::*;

    // all load type instructions
    enum_from_primitive!{
        #[derive(Debug, PartialEq)]
        pub enum InstLoad {
            LB,
            LH,
            LW,
            LD,
            LQ,
            LBU,
            LHU,
            LWU,
            LDU,
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
            SQ
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
        pub regs: [u128; 32],
        pub pc: u128,

        // user memory
        pub mem: Vec<u128>,
    }

    impl Module {
        pub fn new() -> Self {
            Self {
                regs: [0; 32],
                pc: 0,
                mem: Vec::with_capacity(256)
            }
        }

        pub fn load(&mut self, inst: u32) {
            let InstI { rd, rs1, imm_11_7, imm_6_0, funct3, opcode } = extract_i(inst);
            let addr = self.regs[rs1] as usize + (imm_11_7 << 6 + imm_6_0) as usize;
            
            match InstLoad::from_u32(funct3).unwrap() {
                InstLoad::LB => self.regs[rd] = self.mem[addr] as i8 as u128,
                InstLoad::LH => self.regs[rd] = self.mem[addr] as i16 as u128,
                InstLoad::LW => self.regs[rd] = self.mem[addr] as i32 as u128,
                InstLoad::LD => self.regs[rd] = self.mem[addr] as i64 as u128,
                InstLoad::LQ => self.regs[rd] = self.mem[addr] as i128 as u128,

                InstLoad::LBU => self.regs[rd] = self.mem[addr] as u8 as u128,
                InstLoad::LHU => self.regs[rd] = self.mem[addr] as u16 as u128,
                InstLoad::LWU => self.regs[rd] = self.mem[addr] as u32 as u128,
                InstLoad::LDU => self.regs[rd] = self.mem[addr] as u64 as u128,
            }
        }

        pub fn store(&mut self, inst: u32) {
            let InstB { imm_11_7, rs1, rs2, imm_6_0, funct3, opcode } = extract_b(inst);

            let addr = self.regs[rs1] as usize + (imm_11_7 << 6 + imm_6_0) as usize;

            match InstStore::from_u32(funct3).unwrap() {
                InstStore::SB => self.mem[addr] &= self.regs[rs2] as i8 as u128,
                InstStore::SH => self.mem[addr] &= self.regs[rs2] as i16 as u128,
                InstStore::SW => self.mem[addr] &= self.regs[rs2] as i32 as u128,
                InstStore::SD => self.mem[addr] &= self.regs[rs2] as i64 as u128,
                InstStore::SQ => self.mem[addr] &= self.regs[rs2] as i128 as u128,
            }
        }
    }
}

pub struct Machine {
    pub modi: Option<rv128i::Module>
}

impl Machine {
    pub fn new() -> Self {
        Self { modi: Some(rv128i::Module::new()) }
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

            MajorOpcodes::OpImm => todo!(),
            MajorOpcodes::OpImm32 => todo!(),
            MajorOpcodes::Amo => todo!(),
            MajorOpcodes::MiscMem => todo!(),
            MajorOpcodes::Op => todo!(),
            MajorOpcodes::Lui => todo!(),
            MajorOpcodes::Op32 => todo!(),
            MajorOpcodes::MAdd => todo!(),
            MajorOpcodes::MSub => todo!(),
            MajorOpcodes::NMSub => todo!(),
            MajorOpcodes::NMAdd => todo!(),
            MajorOpcodes::OpFp => todo!(),
            MajorOpcodes::System => todo!(),
            
            // control transfer
            MajorOpcodes::J => todo!(),
            MajorOpcodes::Jal => todo!(),
            MajorOpcodes::Jalr => todo!(),
            MajorOpcodes::Branch => todo!(),

            _ => panic!("Unknowwn Opcode")
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
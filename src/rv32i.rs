use std::rc::Rc;

use bit::BitIndex;
use enum_primitive::enum_from_primitive;

use crate::rvcore::MemRW;

type XTYPE = u32;
const XLEN: usize = 32;

// common instruction types

pub trait InstImm {
    fn imm(&self) -> XTYPE;
}

pub struct InstR {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub rs2: XTYPE,
    pub funct7: XTYPE
}

pub struct InstI {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub imm_11_0: XTYPE
}

impl InstImm for InstI {
    fn imm(&self) -> XTYPE {
        self.imm_11_0
    }
}

pub struct InstS {
    pub opcode: XTYPE,
    pub imm_4_0: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub rs2: XTYPE,
    pub imm_11_5: XTYPE
}

impl InstImm for InstS {
    fn imm(&self) -> XTYPE {
        self.imm_4_0 + self.imm_11_5 << 5
    }
}

pub struct InstB {
    pub opcode: XTYPE,
    pub imm11: XTYPE,
    pub imm_4_1: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub rs2: XTYPE,
    pub imm_10_5: XTYPE,
    pub imm_12: XTYPE
}

impl InstImm for InstB {
    fn imm(&self) -> XTYPE {
        self.imm_4_1 << 1 + self.imm_10_5 << 5 + self.imm11 << 11 + self.imm_12 << 12
    }
}

pub struct InstU {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub imm_31_12: XTYPE
}

impl InstImm for InstU {
    fn imm(&self) -> XTYPE {
        self.imm_31_12 << 12
    }
}

pub struct InstJ {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub imm_19_12: XTYPE,
    pub imm11: XTYPE,
    pub imm_10_1: XTYPE,
    pub imm20: XTYPE
}

impl InstImm for InstJ {
    fn imm(&self) -> XTYPE {
        self.imm_10_1 << 1 + self.imm11 << 11 + self.imm_19_12 << 12 + self.imm20 << 20
    }
}

pub trait Inst {
    fn inst_r(&self) -> InstR;
    fn inst_i(&self) -> InstI;
    fn inst_s(&self) -> InstS;
    fn inst_b(&self) -> InstB;
    fn inst_u(&self) -> InstU;
    fn inst_j(&self) -> InstJ;
}

impl Inst for XTYPE {
    fn inst_r(&self) -> InstR {
        InstR {
            opcode: self.bit_range(0..7),
            rd: self.bit_range(7..12),
            funct3: self.bit_range(12..15),
            rs1: self.bit_range(15..20),
            rs2: self.bit_range(20..25),
            funct7: self.bit_range(25..32)
        }
    }

    fn inst_i(&self) -> InstI {
        InstI {
            opcode: self.bit_range(0..7),
            rd: self.bit_range(7..12),
            funct3: self.bit_range(12..15),
            rs1: self.bit_range(15..20),
            imm_11_0: self.bit_range(20..32)
        }
    }

    fn inst_s(&self) -> InstS {
        InstS {
            opcode: self.bit_range(0..7),
            imm_4_0: self.bit_range(7..12),
            funct3: self.bit_range(12..15),
            rs1: self.bit_range(15..20),
            rs2: self.bit_range(20..25),
            imm_11_5: self.bit_range(25..32)
        }
    }

    fn inst_b(&self) -> InstB {
        InstB {
            opcode: self.bit_range(0..7),
            imm11: self.bit(7) as XTYPE,
            imm_4_1: self.bit_range(8..12),
            funct3: self.bit_range(12..15),
            rs1: self.bit_range(15..20),
            rs2: self.bit_range(20..25),
            imm_10_5: self.bit_range(25..31),
            imm_12: self.bit(32) as XTYPE
        }
    }

    fn inst_u(&self) -> InstU {
        InstU {
            opcode: self.bit_range(0..7),
            rd: self.bit_range(7..12),
            imm_31_12: self.bit_range(12..32)
        }
    }

    fn inst_j(&self) -> InstJ {
        InstJ {
            opcode: self.bit_range(0..7),
            rd: self.bit_range(7..12),
            imm_19_12: self.bit_range(12..20),
            imm11: self.bit(20) as XTYPE,
            imm_10_1: self.bit_range(21..31),
            imm20: self.bit(32) as XTYPE
        }
    }
}

// less common instruction variants

pub struct InstFence {
    opcode: XTYPE,
    rd: XTYPE,
    funct3: XTYPE,
    rs1: XTYPE,
    sw: bool,
    sr: bool,
    so: bool,
    si: bool,
    pw: bool,
    pr: bool,
    po: bool,
    pi: bool,
    fm: XTYPE
}

pub trait InstFenceTrait {
    fn inst_fence(&self) -> InstFence;
}

impl InstFenceTrait for XTYPE {
    fn inst_fence(&self) -> InstFence {
        InstFence {
            opcode: self.bit_range(0..7),
            rd: self.bit_range(7..12),
            funct3: self.bit_range(12..15),
            rs1: self.bit_range(15..20),
            sw
        }
    }
}

// opcodes

enum_from_primitive!{
    pub enum Opcodes {
        Load        = 0b00000,
        LoadFp      = 0b00001,
        MiscMem     = 0b00011,
        OpImm       = 0b00100,
        Auipc       = 0b00101,
        OpImm32     = 0b00110,
        Store       = 0b01000,
        StoreFp     = 0b01001,
        Amo         = 0b01011,
        Op          = 0b01100,
        Lui         = 0b01101,
        Op32        = 0b01110,
        Madd        = 0b10000,
        MSub        = 0b10001,
        NMSub       = 0b10010,
        NMAdd       = 0b10011,
        OpFp        = 0b10100,
        Branch      = 0b11000,
        Jalr        = 0b11001,
        Jal         = 0b11011,
        System      = 0b11100,
    }
}

// reference module

pub struct RefMod {
    // regs: x0 - x31:
    // x0 -- zero -- (hardwired) 0
    // x1 -- ra -- return address
    // x2 -- sp -- stack pointer
    // x3 -- gp -- global pointer
    // x4 -- tp -- thread pointer
    // x5 -- t0 -- alternate link address
    // x6-7 -- t1-2 -- temporaries
    // x8 -- s0/fp -- saved register / frame pointer
    // x9 -- s1 -- saved register
    // x10-11 -- a0-1 -- function arguments / return vaulues
    // x12-17 -- a2-7 -- function arguments
    // x18-27 -- s2-11 -- saved registers
    // x28-31 -- t3-6 -- temporaries
    pub regs: [XTYPE; 32],

    // byte addressed pc
    pub pc: XTYPE,

    // handles
    pub opcodes: Vec<Opcodes>,

    // memory interface reference
    pub mem: Rc<dyn MemRW>
}

impl RefMod {
    pub fn new(mem: Rc<dyn MemRW>) -> Self {
        Self { 
            regs: [0; 32],
            pc: 0,
            opcodes: vec![
                Opcodes::Lui,
                Opcodes::Auipc,
                Opcodes::Jal,
                Opcodes::Jalr,
                Opcodes::Branch,
                Opcodes::Load,
                Opcodes::Store,
                Opcodes::OpImm,
                Opcodes::Op,
                Opcodes::MiscMem,
                Opcodes::System
            ],
            mem
        }
    }
}

macro_rules! gen_op_fn {
    // regular syntax

    () => {

    };

    ($name:ident = $inst_t:ty) => {
        fn $name(&mut self, inst: $inst_t);
    };

    ($name:ident = $inst_t:ty, $($rest:tt)*) => {
        gen_op_fn!($name=$inst_t);
        gen_op_fn!($($rest)*);
    };

    // enhanced syntax [fn1, fn2, fn3] = type
    
    ([] = $inst_t:ty) => {
        // nothing
    };

    ([$name:ident] = $inst_t:ty) => {
        gen_op_fn!($name=$inst_t);
    };

    ([$name:ident, $($list:tt)*] = $inst_t:ty) => {
        gen_op_fn!($name=$inst_t);
        gen_op_fn!([$($list)*]=$inst_t);
    };

    ([$($list:tt)*] = $inst_t:ty, $($rest:tt)*) => {
        gen_op_fn!([$($list)*]=$inst_t);
        gen_op_fn!($($rest)*);
    };
}

macro_rules! hint_if {
    ($name:expr, $cond:expr) => {
        if $cond {
            println!("Hint: {}", $name);
            return;
        }
    };
}

pub trait RefTrait32 {
    gen_op_fn!(
        // opcodes have extra 11 at end for 16b distinction stuff...
        lui=    InstU,// 0b0110111
        auipc=  InstU,// 0b0010111
        jal=    InstJ,// 0b1101111
        jalr=   InstI,// 0b1100111
        [beq, bne, blt, bge, bltu, bgeu]= InstB,// 0b1100011
        [lb, lh, lw, lbu, lhu] = InstI,// 0b0000011
        [sb, sh, sw] = InstS,// 0b0100011
        [addi, slti, sltiu, xori, ori, andi] = InstI,// 0b0010011
        [slli, srli, srai] = InstI,// 0b0010011
        [add, sub, sll, slt, sltu, xor, srl, sra, or, and] = InstR,// 0b0110011
        // fence=  Different...,// 0b0001111
        // ecall=  Different...,// 0b1110011
        // ebreak= Different...,// 0b1110011
    );
}

impl RefTrait32 for RefMod {
    fn lui(&mut self, inst: InstU) {
        hint_if!("LUI", inst.rd == 0);

        self.regs[inst.rd as usize] = inst.imm();
    }

    fn auipc(&mut self, inst: InstU) {
        hint_if!("AUIPC", inst.rd == 0);
        
        self.regs[inst.rd as usize] = inst.imm() + self.pc;
    }

    fn jal(&mut self, inst: InstJ) {
        let off = inst.imm();
        if off & 0b11 != 0 {
            // instruction-address-misaligned exception
            todo!()
        }
        if inst.rd != 0 {
            self.regs[inst.rd as usize] = self.pc + 4;
        }
        if inst.rd == 1 || inst.rd == 5 {
            // push addr to ra stack
        }
        self.pc += off;
    }

    fn jalr(&mut self, inst: InstI) {
        let mut addr = inst.imm() + inst.rs1;
        // last bit does not matter as it will be set 0
        if addr & 0b10 == 0 {
            // instruction-address-misaligned exception
            todo!()
        }
        addr.set_bit(0, false);

        if inst.rd != 0 {
            self.regs[inst.rd as usize] = self.pc + 4;
        }

        let link_rd = inst.rd == 1 || inst.rd == 5;
        let link_rs1 = inst.rs1 == 1 || inst.rs1 == 5;

        if link_rd {
            if link_rs1 {
                if inst.rd != inst.rs1 {
                    // push addr to ra stack
                }
                else {
                    // pop and push
                }
            }
            else {
                // push addr to ra stack
            }
        }
        else {
            if link_rs1 {
                // pop
            }
        }

        self.pc = addr;
    }

    fn beq(&mut self, inst: InstB) {
        if inst.rs1 == inst.rs2 {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn bne(&mut self, inst: InstB) {
        if inst.rs1 != inst.rs2 {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn blt(&mut self, inst: InstB) {
        if (inst.rs1 as i32) < (inst.rs2 as i32) {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn bge(&mut self, inst: InstB) {
        if (inst.rs1 as i32) >= (inst.rs2 as i32) {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn bltu(&mut self, inst: InstB) {
        if inst.rs1 < inst.rs2 {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn bgeu(&mut self, inst: InstB) {
        if inst.rs1 >= inst.rs2 {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn lb(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.read_u8(addr as usize) as i8 as XTYPE;
    }

    fn lh(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.read_u16(addr as usize) as i16 as XTYPE;
    }

    fn lw(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.read_u32(addr as usize) as i32 as XTYPE;
    }

    fn lbu(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.read_u8(addr as usize) as u8 as XTYPE;
    }

    fn lhu(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.read_u16(addr as usize) as u16 as XTYPE;
    }

    fn sb(&mut self, inst: InstS) {
        let addr = inst.rs1 + inst.imm();
        self.mem.write_u8(addr as usize, (self.regs[inst.rs2 as usize] & 0xff) as u8);
    }

    fn sh(&mut self, inst: InstS) {
        let addr = inst.rs1 + inst.imm();
        let hw = (self.regs[inst.rs2 as usize] & 0xff) as u16 +
            (self.regs[inst.rs2 as usize] & (0xff << 8)) as u16;
        self.mem.write_u16(addr as usize, hw);
    }

    fn sw(&mut self, inst: InstS) {
        let addr = inst.rs1 + inst.imm();
        let w = (self.regs[inst.rs2 as usize] & 0xff) as u32 +
            (self.regs[inst.rs2 as usize] & (0xff << 8)) as u32 + 
            (self.regs[inst.rs2 as usize] & (0xff << 16)) as u32 + 
            (self.regs[inst.rs2 as usize] & (0xff << 24)) as u32;
        self.mem.write_u32(addr as usize, w);
    }

    fn addi(&mut self, inst: InstI) {
        todo!()
    }

    fn slti(&mut self, inst: InstI) {
        todo!()
    }

    fn sltiu(&mut self, inst: InstI) {
        todo!()
    }

    fn xori(&mut self, inst: InstI) {
        todo!()
    }

    fn ori(&mut self, inst: InstI) {
        todo!()
    }

    fn andi(&mut self, inst: InstI) {
        todo!()
    }

    fn slli(&mut self, inst: InstI) {
        todo!()
    }

    fn srli(&mut self, inst: InstI) {
        todo!()
    }

    fn srai(&mut self, inst: InstI) {
        todo!()
    }

    fn add(&mut self, inst: InstR) {
        todo!()
    }

    fn sub(&mut self, inst: InstR) {
        todo!()
    }

    fn sll(&mut self, inst: InstR) {
        todo!()
    }

    fn slt(&mut self, inst: InstR) {
        todo!()
    }

    fn sltu(&mut self, inst: InstR) {
        todo!()
    }

    fn xor(&mut self, inst: InstR) {
        todo!()
    }

    fn srl(&mut self, inst: InstR) {
        todo!()
    }

    fn sra(&mut self, inst: InstR) {
        todo!()
    }

    fn or(&mut self, inst: InstR) {
        todo!()
    }

    fn and(&mut self, inst: InstR) {
        todo!()
    }
}
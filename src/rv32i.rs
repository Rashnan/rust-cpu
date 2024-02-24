use std::{cell::RefCell, rc::Rc};

use bit::BitIndex;
use enum_primitive::{enum_from_primitive, FromPrimitive};

use crate::rvcore::{EeiCore, MemRW};

type XTYPE = u32;
type ITYPE = i32;
// const XLEN: usize = 32;

// common instruction types

// full instruction into xtype
pub trait InstXType {
    fn to_xtype(&self) -> XTYPE;
}

// just the immediate as xtype
pub trait InstImm {
    fn imm(&self) -> XTYPE;
}

#[derive(Debug)]
pub struct InstR {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub rs2: XTYPE,
    pub funct7: XTYPE
}

impl InstXType for InstR {
    fn to_xtype(&self) -> XTYPE {
        self.opcode + (self.rd << 7) + (self.funct3 << 12) + (self.rs1 << 15) + (self.rs2 << 20) + (self.funct7 << 25)
    }
}

#[derive(Debug)]
pub struct InstI {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub imm_11_0: XTYPE
}

impl InstXType for InstI {
    fn to_xtype(&self) -> XTYPE {
        self.opcode + (self.rd << 7) + (self.funct3 << 12) + (self.rs1 << 15) + (self.imm_11_0 << 20)
    }
}

impl InstImm for InstI {
    fn imm(&self) -> XTYPE {
        self.imm_11_0
    }
}

#[derive(Debug)]
pub struct InstS {
    pub opcode: XTYPE,
    pub imm_4_0: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub rs2: XTYPE,
    pub imm_11_5: XTYPE
}

impl InstXType for InstS {
    fn to_xtype(&self) -> XTYPE {
        self.opcode + (self.imm_4_0 << 7) + (self.funct3 << 12) + (self.rs1 << 15) + (self.rs2 << 20) + (self.imm_11_5 << 25)
    }
}

impl InstImm for InstS {
    fn imm(&self) -> XTYPE {
        self.imm_4_0 + (self.imm_11_5 << 5)
    }
}

#[derive(Debug)]
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

impl InstXType for InstB {
    fn to_xtype(&self) -> XTYPE {
        self.opcode + (self.imm11 << 7) + (self.imm_4_1 << 8) + (self.funct3 << 12) + (self.rs1 << 15) + (self.rs2 << 20) + (self.imm_10_5 << 25) + (self.imm_12 << 31)
    }
}

impl InstImm for InstB {
    fn imm(&self) -> XTYPE {
        (self.imm_4_1 << 1) + (self.imm_10_5 << 5) + (self.imm11 << 11) + (self.imm_12 << 12)
    }
}

#[derive(Debug)]
pub struct InstU {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub imm_31_12: XTYPE
}

impl InstXType for InstU {
    fn to_xtype(&self) -> XTYPE {
        self.opcode + (self.rd << 7) + (self.imm_31_12 << 12)
    }
}


impl InstImm for InstU {
    fn imm(&self) -> XTYPE {
        self.imm_31_12 << 12
    }
}

#[derive(Debug)]
pub struct InstJ {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub imm_19_12: XTYPE,
    pub imm11: XTYPE,
    pub imm_10_1: XTYPE,
    pub imm20: XTYPE
}

impl InstXType for InstJ {
    fn to_xtype(&self) -> XTYPE {
        self.opcode + (self.rd << 7) + (self.imm_19_12 << 12) + (self.imm11 << 20) + (self.imm_10_1 << 21) + (self.imm20 << 31)
    }
}

impl InstImm for InstJ {
    fn imm(&self) -> XTYPE {
        (self.imm_10_1 << 1) + (self.imm11 << 11) + (self.imm_19_12 << 12) + (self.imm20 << 20)
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
        let ret = InstS {
            opcode: self.bit_range(0..7),
            imm_4_0: self.bit_range(7..12),
            funct3: self.bit_range(12..15),
            rs1: self.bit_range(15..20),
            rs2: self.bit_range(20..25),
            imm_11_5: self.bit_range(25..32)
        };
        ret
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

// less common instruction derivatives

pub struct InstFence {
    pub opcode: XTYPE,
    pub rd: XTYPE,
    pub funct3: XTYPE,
    pub rs1: XTYPE,
    pub sw: bool,
    pub sr: bool,
    pub so: bool,
    pub si: bool,
    pub pw: bool,
    pub pr: bool,
    pub po: bool,
    pub pi: bool,
    pub fm: XTYPE
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
            sw: self.bit(20),
            sr: self.bit(21),
            so: self.bit(22),
            si: self.bit(23),
            pw: self.bit(24),
            pr: self.bit(25),
            po: self.bit(26),
            pi: self.bit(27),
            fm: self.bit_range(28..32) as XTYPE
        }
    }
}

// opcodes

// main
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

// branch
enum_from_primitive!{
    pub enum OpcodeBranch {
        BEQ,
        BNE,
        BLT=0b100,
        BGE,
        BLTU,
        BGEU
    }
}

// load
enum_from_primitive!{
    pub enum OpcodeLoad {
        LB,
        LH,
        LW,
        LBU=0b100,
        LHU
    }
}

// store
enum_from_primitive!{
    pub enum OpcodeStore {
        SB,
        SH,
        SW
    }
}

// op imm
enum_from_primitive!{
    pub enum OpcodeOpImm {
        ADDI,
        SLTI=0b10,
        SLTIU,
        XORI=0b100,
        ORI=0b110,
        ANDI=0b111,
        SLLI=0b001,
        SXLI=0b101,
    }
}

// op
enum_from_primitive!{
    pub enum OpcodeOp {
        ADD,// and SUB
        SLL,
        SLT,
        SLTU,
        XOR,
        SRX,
        OR,
        AND
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

    // opcode traps
    // pub traps: HashMap<u32, Box<Fn(&mut RefMod, u32)>>,

    // memory interface reference
    pub mem: Rc<RefCell<dyn MemRW>>
}

impl EeiCore for RefMod {
    fn new(mem: Rc<RefCell<dyn MemRW>>) -> Self {
        Self { 
            regs: [0; 32],
            pc: 0,
            mem
        }
    }

    fn trap(&mut self, inst: u32) -> bool {
        let opcode = inst.bit_range(0..7);

        match Opcodes::from_u32(opcode).unwrap() {
            Opcodes::Lui => {
                self.lui(inst.inst_u());
                true
            },
            Opcodes::Auipc => {
                self.auipc(inst.inst_u());
                true
            },
            Opcodes::Jal => {
                self.jal(inst.inst_j());
                true
            },
            Opcodes::Jalr => {
                self.jalr(inst.inst_i());
                true
            },
            Opcodes::Branch => {
                let inst = inst.inst_b();
                match OpcodeBranch::from_u32(inst.funct3).unwrap() {
                    OpcodeBranch::BEQ => self.beq(inst),
                    OpcodeBranch::BNE => self.bne(inst),
                    OpcodeBranch::BLT => self.blt(inst),
                    OpcodeBranch::BGE => self.bge(inst),
                    OpcodeBranch::BLTU => self.bltu(inst),
                    OpcodeBranch::BGEU => self.bgeu(inst),
                }
                true
            },
            Opcodes::Load => {
                let inst = inst.inst_i();
                match OpcodeLoad::from_u32(inst.funct3).unwrap() {
                    OpcodeLoad::LB => self.lb(inst),
                    OpcodeLoad::LH => self.lh(inst),
                    OpcodeLoad::LW => self.lw(inst),
                    OpcodeLoad::LBU => self.lbu(inst),
                    OpcodeLoad::LHU => self.lhu(inst),
                }
                true
            },
            Opcodes::Store => {
                let inst = inst.inst_s();
                match OpcodeStore::from_u32(inst.funct3).unwrap() {
                    OpcodeStore::SB => self.sb(inst),
                    OpcodeStore::SH => self.sh(inst),
                    OpcodeStore::SW => self.sw(inst),
                }
                true
            },
            Opcodes::OpImm => {
                let inst = inst.inst_i();
                match OpcodeOpImm::from_u32(inst.funct3).unwrap() {
                    OpcodeOpImm::ADDI => self.addi(inst),
                    OpcodeOpImm::SLTI => self.slti(inst),
                    OpcodeOpImm::SLTIU => self.sltiu(inst),
                    OpcodeOpImm::XORI => self.xori(inst),
                    OpcodeOpImm::ORI => self.ori(inst),
                    OpcodeOpImm::ANDI => self.andi(inst),
                    OpcodeOpImm::SLLI => self.slli(inst),
                    OpcodeOpImm::SXLI => {
                        if inst.imm_11_0.bit(11) {
                            self.srai(inst)
                        }
                        else {
                            self.srli(inst)
                        }
                    },
                }
                true
            },
            Opcodes::Op => {
                let inst = inst.inst_r();
                match OpcodeOp::from_u32(inst.funct3).unwrap() {
                    OpcodeOp::ADD => {
                        if inst.funct7.bit(6) {
                            self.sub(inst)
                        }
                        else {
                            self.add(inst)
                        }
                    },
                    OpcodeOp::SLL => self.sll(inst),
                    OpcodeOp::SLT => self.slt(inst),
                    OpcodeOp::SLTU => self.sltu(inst),
                    OpcodeOp::XOR => self.xor(inst),
                    OpcodeOp::SRX => {
                        if inst.funct7.bit(6) {
                            self.sra(inst)
                        }
                        else {
                            self.srl(inst)
                        }
                    },
                    OpcodeOp::OR => self.or(inst),
                    OpcodeOp::AND => self.add(inst),
                }
                true
            },
            Opcodes::MiscMem => {
                self.fence(inst.inst_fence());
                true
            },
            Opcodes::System => {
                let inst = inst.inst_i();
                if inst.imm_11_0.bit(0) {
                    self.ebreak(inst)
                }
                else {
                    self.ecall(inst)
                }
                true
            },
            _ => false
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
        fence=  InstFence,// 0b0001111
        ecall=  InstI,// 0b1110011
        ebreak= InstI,// 0b1110011
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
        if (inst.rs1 as ITYPE) < (inst.rs2 as ITYPE) {
            let off = inst.imm();
            if off & 0b11 != 0 {
                // instruction-address-misaligned exception
                todo!()
            }
            self.pc += off;
        }
    }

    fn bge(&mut self, inst: InstB) {
        if (inst.rs1 as ITYPE) >= (inst.rs2 as ITYPE) {
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
        self.regs[inst.rd as usize] = self.mem.borrow().read_u8(addr as usize) as i8 as XTYPE;
    }

    fn lh(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.borrow().read_u16(addr as usize) as i16 as XTYPE;
    }

    fn lw(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.borrow().read_u32(addr as usize) as i32 as XTYPE;
    }

    fn lbu(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.borrow().read_u8(addr as usize) as u8 as XTYPE;
    }

    fn lhu(&mut self, inst: InstI) {
        if inst.rd == 0 {
            // saving to x0
            todo!()
        }
        let addr = inst.rs1 + inst.imm();
        self.regs[inst.rd as usize] = self.mem.borrow().read_u16(addr as usize) as u16 as XTYPE;
    }

    fn sb(&mut self, inst: InstS) {
        let addr = inst.rs1 + inst.imm();
        self.mem.borrow_mut().write_u8(addr as usize, (self.regs[inst.rs2 as usize] & 0xff) as u8);
    }

    fn sh(&mut self, inst: InstS) {
        let addr = inst.rs1 + inst.imm();
        let hw = (self.regs[inst.rs2 as usize] & 0xff) as u16 +
            (self.regs[inst.rs2 as usize] & (0xff << 8)) as u16;
        self.mem.borrow_mut().write_u16(addr as usize, hw);
    }

    fn sw(&mut self, inst: InstS) {
        let addr = inst.rs1 + inst.imm();
        let w = (self.regs[inst.rs2 as usize] & 0xff) as u32 +
            (self.regs[inst.rs2 as usize] & (0xff << 8)) as u32 + 
            (self.regs[inst.rs2 as usize] & (0xff << 16)) as u32 + 
            (self.regs[inst.rs2 as usize] & (0xff << 24)) as u32;
        self.mem.borrow_mut().write_u32(addr as usize, w);
    }

    fn addi(&mut self, inst: InstI) {
        hint_if!("ADDI", inst.rd == 0 && (inst.rs1 != 0 || inst.imm() != 0));
        self.regs[inst.rd as usize] = inst.imm() + self.regs[inst.rs1 as usize];
    }

    fn slti(&mut self, inst: InstI) {
        hint_if!("SLTI", inst.rd == 0);
        self.regs[inst.rd as usize] = if (self.regs[inst.rs1 as usize] as ITYPE) < (inst.imm() as ITYPE) { 1 } else { 0 };
    }

    fn sltiu(&mut self, inst: InstI) {
        hint_if!("SLTIU", inst.rd == 0);
        self.regs[inst.rd as usize] = if self.regs[inst.rs1 as usize] < inst.imm() { 1 } else { 0 };
    }

    fn xori(&mut self, inst: InstI) {
        hint_if!("XORI", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] ^ inst.imm();
    }

    fn ori(&mut self, inst: InstI) {
        hint_if!("ORI", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] | inst.imm();
    }

    fn andi(&mut self, inst: InstI) {
        hint_if!("ANDI", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] & inst.imm();
    }

    fn slli(&mut self, inst: InstI) {
        hint_if!("SLLI", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] << (inst.imm() & 0x1f);
    }

    fn srli(&mut self, inst: InstI) {
        hint_if!("SRLI", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] >> (inst.imm() & 0x1f);
    }

    fn srai(&mut self, inst: InstI) {
        hint_if!("SRAI", inst.rd == 0);
        self.regs[inst.rd as usize] = ((self.regs[inst.rs1 as usize] as ITYPE) >> (inst.imm() & 0x1f)) as XTYPE;
    }

    fn add(&mut self, inst: InstR) {
        hint_if!("ADD", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] + self.regs[inst.rs2 as usize];
    }

    fn sub(&mut self, inst: InstR) {
        hint_if!("SUB", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] - self.regs[inst.rs2 as usize];
    }

    fn sll(&mut self, inst: InstR) {
        hint_if!("SLL", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] << self.regs[inst.rs2 as usize].bit_range(0..5);
    }

    fn slt(&mut self, inst: InstR) {
        hint_if!("SLT", inst.rd == 0);
        self.regs[inst.rd as usize] = if (self.regs[inst.rs1 as usize] as ITYPE) < (self.regs[inst.rs2 as usize] as ITYPE) { 1 } else { 0 };
    }

    fn sltu(&mut self, inst: InstR) {
        hint_if!("SLTU", inst.rd == 0);
        self.regs[inst.rd as usize] = if self.regs[inst.rs1 as usize] < self.regs[inst.rs2 as usize] { 1 } else { 0 };
    }

    fn xor(&mut self, inst: InstR) {
        hint_if!("XOR", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] ^ self.regs[inst.rs2 as usize];
    }

    fn srl(&mut self, inst: InstR) {
        hint_if!("SRL", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] >> self.regs[inst.rs2 as usize].bit_range(0..5);
    }

    fn sra(&mut self, inst: InstR) {
        hint_if!("SRA", inst.rd == 0);
        self.regs[inst.rd as usize] = ((self.regs[inst.rs1 as usize] as ITYPE) >> self.regs[inst.rs2 as usize].bit_range(0..5)) as XTYPE;
    }

    fn or(&mut self, inst: InstR) {
        hint_if!("OR", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] | self.regs[inst.rs2 as usize];
    }

    fn and(&mut self, inst: InstR) {
        hint_if!("AND", inst.rd == 0);
        self.regs[inst.rd as usize] = self.regs[inst.rs1 as usize] & self.regs[inst.rs2 as usize];
    }

    fn fence(&mut self, inst: InstFence) {
        let pred = inst.pi | inst.po | inst.pr | inst.pw;
        let succ = inst.si | inst.so | inst.sr | inst.sw;
        hint_if!("Fence", !pred || !succ);
        todo!();
    }

    fn ecall(&mut self, inst: InstI) {
        // call trap
        // EEI defines how to pass params
        let _priv = inst.funct3;
        todo!()
    }

    fn ebreak(&mut self, inst: InstI) {
        // break out of trap
        // EEI defines how to pass params
        let _priv = inst.funct3;
        todo!()
    }
}
// https://inst.eecs.berkeley.edu/~cs61c/resources/su18_lec/Lecture7.pdf

// basic instruction set rv32i or rv64i
// R -- 3 regs
// I -- imm/load (12b)
// S -- store
// SB -- branch
// U -- upper imm (20b)
// UJ -- jump

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

pub mod rv32i {
    pub enum InstructionFormat {
        R, I, S, B, U, J
    }

    pub struct Machine {
        // referred to as xN where N is the number
        // x0 == 0
        // x1-x31 are general purpose
        // x1 -- ra -- return address
        // x2 -- sp -- stack pointer
        // x3 -- gp -- global pointer
        // x4 -- tp -- thread local storage
        // x5 - x11 or t0 - t6 -- temp calc vals
        // x10 - x17 or a0 - a7 -- fn arg regs
        // x8, x9, x18 - x27 or s0 - s11 -- saved regs for preserving vals across fn calls
        // x28 - x31 or t3 - t6 -- additional temp regs
        pub regs: [u32; 32],
    }

    impl Machine {
        pub fn new() -> Self {
            Self { regs: [0; 32] }
        }

        pub fn process(self, instruction: u32) {
            let opcode = 
        }
    }
}

pub mod rvf {
    pub struct Machine {
        // ft0 - ft7 or f0 - f7 -- temp intermediate regs
        // fs0 - fs11 or f8, f9, f18 - f27 -- preversing vals across fn calls
        // fa0 - fa7 or f10 - f17 -- fn arg regs
        // ft8 - ft11 or f28 - f31  -- additional temp regs
        pub regs: [f32; 32]
    }
}
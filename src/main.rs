use std::collections::HashMap;

use rust_cpu::{
	rv32i::{
		InstI,InstR,InstS,InstJ,InstU,
		InstXType,
		OpcodeLoad,
		OpcodeOp,
		OpcodeStore,
		Opcodes
	},
	rvcore::{
		Eei,
		MemRW
	}, asm
};

fn main() {
	let mut cpu = Eei::new();

	let inst_rng = {
		let mem = &mut cpu.mem.borrow_mut();

		// 100+ is data 
		mem.data[100] = 0x1;
		mem.data[101] = 0x2;

		// 0-3 is cmds

		let insts = asm!(@compile
			lb x(1), x(0), 100;
			lb x(2), x(0), 101;
			add x(1), x(1), x(2);
			sb x(1), x(0), 102;
			// lui x(1), 100;
		);

		for (i, inst) in insts.iter().enumerate() {
			mem.write_u32(i * 4, *inst);
		}

		0..(insts.len() as u32 * 4)
	};

	// run

	cpu.run_range(inst_rng);

	{
		let data = &cpu.mem.borrow().data;
		println!("final data: {}", data[102]);
		// println!("final data: 0x{:08x}", cpu.mem.borrow().read_u32(0x100));
	}
}
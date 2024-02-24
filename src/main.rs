use rust_cpu::{rv32i::{InstI, InstR, InstS, InstXType, OpcodeLoad, OpcodeOp, OpcodeStore, Opcodes}, rvcore::{Eei, MemRW}};

fn main() {
	let mut cpu = Eei::new();

	{
		let mem = &mut cpu.mem.borrow_mut();

		// 100+ is data 
		mem.data[100] = 0x1;
		mem.data[101] = 0x2;
		// data[102] = 0x1 + 0x2 = 0x3;

		// 0-3 is cmds
		
		let insts = vec![
			// build assmebler
			InstI {
			    opcode: Opcodes::Load as u32,
			    rd: 1,
			    funct3: OpcodeLoad::LB as u32,
			    rs1: 0,
			    imm_11_0: 100,
			}.to_xtype(),
			InstI {
			    opcode: Opcodes::Load as u32,
			    rd: 2,
			    funct3: OpcodeLoad::LB as u32,
			    rs1: 0,
			    imm_11_0: 101,
			}.to_xtype(),
			InstR {
				opcode: Opcodes::Op as u32,
				rd: 1,
				funct3: OpcodeOp::ADD as u32,
				rs1: 1,
				rs2: 2,
				funct7: 0
			}.to_xtype(),
			InstS {
				opcode: Opcodes::Store as u32,
				imm_4_0: 102 & !(((-1i8) as u32) << 5),
				funct3: OpcodeStore::SB as u32,
				rs1: 0,
				rs2: 1,
				imm_11_5: 102 >> 5
			}.to_xtype()
		];

		for (i, inst) in insts.iter().enumerate() {
			mem.write_u32(i * 4, *inst);
		}
	}

	// run

	cpu.run_range(0..16);

	{
		let data = &cpu.mem.borrow().data;
		println!("final data: {}", data[102]);
	}
}
const OpInfo OPCODE_TABLE[OPCODE_SIZE] =
{
    {op_group::reg_t,"mov",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"add",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"sub",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"mul",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"div",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"mod",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},


    {op_group::reg_t,"lsl",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"asr",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"lsr",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},

    {op_group::reg_t,"xor",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"or",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"and",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"not",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},

    {op_group::reg_t,"sxb",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"sxh",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},

    {op_group::imm_t,"mov",2,{arg_type::dst_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"add",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"sub",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"mul",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::imm_t,"and",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"xor",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::load_t,"lb",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lh",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lw",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::load_t,"lsb",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lsh",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::store_t,"sb",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::store_t,"sh",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::store_t,"sw",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::load_t,"lea",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::reg_t,"push",1,{arg_type::src_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"pop",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::regm_t, "pushm",1,{arg_type::imm,arg_type::none,arg_type::none}},
    {op_group::regm_t, "popm", 1,{arg_type::imm,arg_type::none,arg_type::none}},

    {op_group::branch_t,"call",1,{arg_type::label,arg_type::none,arg_type::none}},
    {op_group::implicit_t,"ret",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::imm_t,"swi",1,{arg_type::imm,arg_type::none,arg_type::none}},

    // compare unsigned
    {op_group::imm_t,"cmpugt",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::reg_t,"cmpult",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpule",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpugt",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpuge",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},


    // compare signed
    {op_group::imm_t,"cmpsgt",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::reg_t,"cmpslt",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpsle",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpsgt",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpsge",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},

    // dont care about sign for equality
    {op_group::reg_t,"cmpeq",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpne",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},

    {op_group::branch_t,"bnc",2,{arg_type::label,arg_type::src_reg,arg_type::none}},
    {op_group::branch_t,"bc",2,{arg_type::label,arg_type::src_reg,arg_type::none}},
    {op_group::branch_t,"b",1,{arg_type::label,arg_type::none,arg_type::none}},

    {op_group::reg_t,"load_arr_len",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    {op_group::reg_t,"load_arr_data",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    {op_group::reg_t,"store_arr_len",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    {op_group::reg_t,"store_arr_data",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    {op_group::reg_t,"arr_index",3,{arg_type::directive,arg_type::directive,arg_type::directive}},
    {op_group::imm_t,"init_arr_idx",3,{arg_type::directive,arg_type::directive,arg_type::directive}},

    // directives
    {op_group::slot_t,"alloc_slot",3,{arg_type::directive,arg_type::none,arg_type::none}},
    {op_group::slot_t,"free_slot",1,{arg_type::directive,arg_type::none,arg_type::none}},
    {op_group::slot_t,"alloc",3,{arg_type::directive,arg_type::directive,arg_type::directive}},
    {op_group::slot_t,"alloc_vla",3,{arg_type::directive,arg_type::directive,arg_type::directive}},

    {op_group::imm_t,"buf_addr",3,{arg_type::directive,arg_type::directive,arg_type::directive}},

    // stores required information when room is exhausted on opcodes
    {op_group::slot_t,"state_dump",3,{arg_type::directive,arg_type::directive,arg_type::directive}},

    {op_group::reg_t,"push_arg",1,{arg_type::directive,arg_type::none,arg_type::none}},

    // perform cleanup after a function call
    // free the stack space for args
    // restore callee saved registers
    {op_group::imm_t,"clean_args",1,{arg_type::directive,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"save_regs",0,{arg_type::none,arg_type::none,arg_type::none}},
    {op_group::implicit_t,"restore_regs",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"exit_block",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"placeholder",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"spill_rv",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::reg_t,"spill",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    {op_group::reg_t,"load",2,{arg_type::directive,arg_type::directive,arg_type::none}},

    {op_group::imm_t,"addrof",3,{arg_type::directive,arg_type::directive,arg_type::directive}},

    // not used
    {op_group::implicit_t,"END",0,{arg_type::none,arg_type::none,arg_type::none}},
};

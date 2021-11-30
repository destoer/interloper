const OpInfo OPCODE_TABLE[OPCODE_SIZE] =
{
    {op_group::reg_t,"mov",2},
    {op_group::reg_t,"add",3},
    {op_group::reg_t,"sub",3},
    {op_group::reg_t,"mul",3},
    {op_group::reg_t,"div",3},
    {op_group::reg_t,"mod",3},


    {op_group::reg_t,"lsl",3},
    {op_group::reg_t,"asr",3},
    {op_group::reg_t,"lsr",3},

    {op_group::reg_t,"xor",3},
    {op_group::reg_t,"or",3},
    {op_group::reg_t,"and",3},
    {op_group::reg_t,"not",1},

    {op_group::reg_t,"sxb",2},
    {op_group::reg_t,"sxh",2},

    {op_group::imm_t,"mov",2},
    {op_group::imm_t,"add",3},
    {op_group::imm_t,"sub",3},

    {op_group::imm_t,"and",3},
    {op_group::imm_t,"xor",3},

    {op_group::load_t,"lb",3},
    {op_group::load_t,"lh",3},
    {op_group::load_t,"lw",3},

    {op_group::load_t,"lsb",3},
    {op_group::load_t,"lsh",3},

    {op_group::load_t,"sb",3},
    {op_group::load_t,"sh",3},
    {op_group::load_t,"sw",3},

    {op_group::reg_t,"push",1},
    {op_group::reg_t,"pop",1},

    {op_group::branch_t,"call",1},
    {op_group::implicit_t,"ret",0},

    {op_group::imm_t,"swi",1},

    // compare unsigned
    {op_group::imm_t,"cmpugt",3},
    {op_group::reg_t,"cmpult",3},
    {op_group::reg_t,"cmpule",3},
    {op_group::reg_t,"cmpugt",3},
    {op_group::reg_t,"cmpuge",3},


    // compare signed
    {op_group::reg_t,"cmpsgt",3},
    {op_group::reg_t,"cmpslt",3},
    {op_group::reg_t,"cmpsle",3},
    {op_group::reg_t,"cmpsgt",3},
    {op_group::reg_t,"cmpsge",3},

    // dont care about sign for equality
    {op_group::reg_t,"cmpeq",3},
    {op_group::reg_t,"cmpne",3},

    {op_group::branch_t,"bnc",2},
    {op_group::branch_t,"bc",2},
    {op_group::branch_t,"b",1},

    // directives
    {op_group::slot_t,"alloc_slot",1},
    {op_group::slot_t,"free_slot",1},
    {op_group::reg_t,"push_arg",1},

    // perform cleanup after a function call
    // free the stack space for args
    // restore callee saved registers
    {op_group::imm_t,"clean_args",1},

    {op_group::implicit_t,"save_regs",0},
    {op_group::implicit_t,"restore_regs",0},

    {op_group::implicit_t,"exit_block",0},

    {op_group::implicit_t,"placeholder",0},

    // ret variable
    {op_group::reg_t,"ret",1},

    // not used
    {op_group::implicit_t,"END",0},
};

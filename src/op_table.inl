constexpr OpInfo OPCODE_TABLE[OPCODE_SIZE] =
{
    // main arith reg3
    {op_group::reg_t,"mov %r, %r",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"mov_unlock %r, %r",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"add %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"sub %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"mul %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"udiv %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"sdiv %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"umod %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"smod %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},


    {op_group::reg_t,"lsl %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"asr %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"lsr %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},

    {op_group::reg_t,"xor %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"or %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"and %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"not %r, %r",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},

    {op_group::reg_t,"sxb %r, %r",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"sxh %r, %r",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"sxw %r, %r",2,{arg_type::dst_reg,arg_type::src_reg,arg_type::none}},

    {op_group::imm_t,"mov %r, %x",2,{arg_type::dst_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"add %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"sub %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"mul %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::imm_t,"lsl %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"lsr %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"and %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"xor %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    // main arith reg2
    {op_group::reg_t,"add %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"sub %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"mul %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"udiv %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"sdiv %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"umod %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"smod %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},


    {op_group::reg_t,"lsl %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"asr %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"lsr %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},

    {op_group::reg_t,"xor %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"or %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"and %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},

    {op_group::imm_t,"add %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"sub %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"mul %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},

    {op_group::imm_t,"lsl %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"lsr %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"and %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},
    {op_group::imm_t,"xor %r, %x",2,{arg_type::dst_src_reg,arg_type::imm,arg_type::none}},

    {op_group::implicit_t,"cqo",0,{arg_type::none,arg_type::none,arg_type::none}},
    {op_group::reg_t,"udiv_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"sdiv_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"umod_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"smod_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},

    {op_group::reg_t,"lsr_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"lsr_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"asr_x86 %r, %r",2,{arg_type::dst_src_reg,arg_type::src_reg,arg_type::none}},

    {op_group::reg_t,"not %r",1,{arg_type::dst_src_reg,arg_type::none,arg_type::none}},

    // float
    {op_group::imm_t,"movf %r, %f",2,{arg_type::dst_float,arg_type::imm,arg_type::none}},
    {op_group::reg_t,"movf %r, %r",2,{arg_type::dst_float,arg_type::src_float,arg_type::none}},
    {op_group::load_t,"lf %r, [%r, %x]",3,{arg_type::dst_float,arg_type::src_reg,arg_type::imm}},
    {op_group::store_t,"sf %r, [%r, %x]",3,{arg_type::src_float,arg_type::src_reg,arg_type::imm}},
    {op_group::reg_t,"addf %r, %r, %r",3,{arg_type::dst_float,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"subf %r, %r, %r",3,{arg_type::dst_float,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"mulf %r, %r, %r",3,{arg_type::dst_float,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"divf %r, %r, %r",3,{arg_type::dst_float,arg_type::src_float,arg_type::src_float}},

    {op_group::reg_t,"cmpflt %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"cmpfle %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"cmpfgt %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"cmpfge %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"cmpfeq %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_float,arg_type::src_float}},
    {op_group::reg_t,"cmpfne %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_float,arg_type::src_float}},

    {op_group::reg_t,"cmp_flags_float %r, %r",2,{arg_type::src_float,arg_type::src_float,arg_type::none}},

    {op_group::reg_t,"setflt %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setfle %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setfgt %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setfge %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::reg_t,"setfeq %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setfne %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::reg_t,"addf %r, %r",2,{arg_type::dst_src_float,arg_type::src_float,arg_type::none}},
    {op_group::reg_t,"subf %r, %r",2,{arg_type::dst_src_float,arg_type::src_float,arg_type::none}},
    {op_group::reg_t,"mulf %r, %r",2,{arg_type::dst_src_float,arg_type::src_float,arg_type::none}},
    {op_group::reg_t,"divf %r, %r",2,{arg_type::dst_src_float,arg_type::src_float,arg_type::none}},

    {op_group::reg_t,"cvtfi %r, %r",2,{arg_type::dst_reg,arg_type::src_float,arg_type::none}},
    {op_group::reg_t,"cvtif %r, %r",2,{arg_type::dst_float,arg_type::src_reg,arg_type::none}},

    // load
    {op_group::load_t,"lb %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lh %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lw %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"ld %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::load_t,"lsb %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lsh %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::load_t,"lsw %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    // store
    {op_group::store_t,"sb %r, [%r, %x]",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::store_t,"sh %r, [%r, %x]",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::store_t,"sw %r, [%r, %x]",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::store_t,"sd %r, [%r, %x]",3,{arg_type::src_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::load_t,"lea %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    {op_group::reg_t,"push %r",1,{arg_type::src_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"pop %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::regm_t, "pushm %m",1,{arg_type::imm,arg_type::none,arg_type::none}},
    {op_group::regm_t, "popm %m", 1,{arg_type::imm,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"call %a",1,{arg_type::label,arg_type::none,arg_type::none}},
    {op_group::reg_t,"call %r",1,{arg_type::src_reg,arg_type::none,arg_type::none}},
    {op_group::implicit_t,"leave",0,{arg_type::none,arg_type::none,arg_type::none}},
    {op_group::implicit_t,"ret",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"syscall",0,{arg_type::none,arg_type::none,arg_type::none}},
    {op_group::imm_t,"swi %x",1,{arg_type::imm,arg_type::none,arg_type::none}},

    // compare unsigned
    {op_group::imm_t,"cmpugt %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::reg_t,"cmpult %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpule %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpugt %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpuge %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},


    // compare signed
    {op_group::imm_t,"cmpsgt %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::reg_t,"cmpslt %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpsle %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpsgt %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpsge %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},

    // dont care about sign for equality
    {op_group::reg_t,"cmpeq %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},
    {op_group::reg_t,"cmpne %r, %r, %r",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::src_reg}},

    {op_group::imm_t,"cmpeq %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},
    {op_group::imm_t,"cmpne %r, %r, %x",3,{arg_type::dst_reg,arg_type::src_reg,arg_type::imm}},

    // compare flags
    {op_group::reg_t,"cmp_flags %r, %r",2,{arg_type::src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::reg_t,"cmp_flags_imm %r, %x",2,{arg_type::src_reg,arg_type::imm,arg_type::none}},

    // flag signed
    {op_group::reg_t,"setslt %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setsle %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setsgt %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setsge %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::reg_t,"setult %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setule %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setugt %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setuge %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::reg_t,"seteq %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"setne %r",1,{arg_type::dst_reg,arg_type::none,arg_type::none}},

    {op_group::branch_t,"bnc %a, %r",2,{arg_type::label,arg_type::src_reg,arg_type::none}},
    {op_group::branch_t,"bc %a, %r",2,{arg_type::label,arg_type::src_reg,arg_type::none}},
    {op_group::branch_t,"b %a",1,{arg_type::label,arg_type::none,arg_type::none}},
    {op_group::branch_reg_t,"b %r",1,{arg_type::src_reg,arg_type::none,arg_type::none}},

    // x86 jump
    {op_group::reg_t,"test %r, %r",2,{arg_type::src_reg,arg_type::src_reg,arg_type::none}},
    {op_group::branch_t,"je %a",1,{arg_type::label,arg_type::none,arg_type::none}},
    {op_group::branch_t,"jne %a",1,{arg_type::label,arg_type::none,arg_type::none}},

    // directives
    {op_group::slot_t,"DIRECTIVE",0,{arg_type::none,arg_type::none,arg_type::none}},
    {op_group::slot_t,"alloc_slot %r, %x",2,{arg_type::directive,arg_type::imm,arg_type::none}},
    
    {op_group::slot_t,"alloc_local_array %r, %x, %x",3,{arg_type::dst_reg,arg_type::imm,arg_type::imm}},
    {op_group::reg_t,"alloc_global_array %r, %x",2,{arg_type::dst_reg,arg_type::imm,arg_type::none}},

    {op_group::reg_t,"push_arg %r",1,{arg_type::src_reg,arg_type::none,arg_type::none}},
    {op_group::reg_t,"push_float_arg %r",1,{arg_type::src_float,arg_type::none,arg_type::none}},

    // perform cleanup after a function call
    // free the stack space for args
    // restore callee saved registers
    {op_group::imm_t, "alloc_stack %x",1,{arg_type::directive,arg_type::none,arg_type::none}},
    {op_group::imm_t, "clean_args %x",1,{arg_type::imm,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"exit_block",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::reg_t,"lock %r",1,{arg_type::directive,arg_type::none,arg_type::none}},
    {op_group::regm_t,"lock_reg_set %m",1,{arg_type::imm,arg_type::none,arg_type::none}},
    {op_group::reg_t,"unlock %r",1,{arg_type::directive,arg_type::none,arg_type::none}},
    {op_group::regm_t,"unlock_reg_set %m",1,{arg_type::imm,arg_type::none,arg_type::none}},

    {op_group::reg_t,"spill %r, %x",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    {op_group::implicit_t,"spill_all",0,{arg_type::none,arg_type::none,arg_type::none}},
    {op_group::implicit_t,"spill_func_bounds",0,{arg_type::none,arg_type::none,arg_type::none}},

    {op_group::implicit_t,"reload_slot %r",1,{arg_type::directive,arg_type::none,arg_type::none}},
    {op_group::implicit_t,"spill_slot %r",1,{arg_type::directive,arg_type::none,arg_type::none}},

    {op_group::reg_t,"load %r, %x",2,{arg_type::directive,arg_type::directive,arg_type::none}},
    

    {op_group::imm_t,"addrof %r, %r, %x",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::reg_t,"load_func_addr %r, %a",2,{arg_type::dst_reg,arg_type::label,arg_type::none}},

    {op_group::load_t,"load_struct_s8 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::load_t,"load_struct_s16 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::load_t,"load_struct_s32 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},

    {op_group::load_t,"load_struct_u8 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::load_t,"load_struct_u16 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::load_t,"load_struct_u32 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::load_t,"load_struct_u64 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},
    {op_group::store_t,"load_struct_f64 %r, [%r, %x]",3,{arg_type::dst_reg,arg_type::directive,arg_type::directive}},

    {op_group::store_t,"store_struct_u8 %r, [%r, %x]",3,{arg_type::src_reg,arg_type::directive,arg_type::directive}},
    {op_group::store_t,"store_struct_u16 %r, [%r, %x]",3,{arg_type::src_reg,arg_type::directive,arg_type::directive}},
    {op_group::store_t,"store_struct_u32 %r, [%r, %x]",3,{arg_type::src_reg,arg_type::directive,arg_type::directive}},
    {op_group::store_t,"store_struct_u64 %r, [%r, %x]",3,{arg_type::src_reg,arg_type::directive,arg_type::directive}},
    {op_group::store_t,"store_struct_f64 %r, [%r, %x]",3,{arg_type::src_reg,arg_type::directive,arg_type::directive}},

    {op_group::slot_t,"pool_addr %r, %x %x",3,{arg_type::dst_reg,arg_type::directive,arg_type::imm}},

    {op_group::reg_t,"load_const_float %r, %x, %f",3,arg_type::dst_reg,arg_type::directive,arg_type::directive},

    {op_group::slot_t,"live_var %r",1,{arg_type::directive,arg_type::none,arg_type::none}},

    // not used
    {op_group::implicit_t,"END",0,{arg_type::none,arg_type::none,arg_type::none}},
};

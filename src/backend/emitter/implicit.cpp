
void emit_implicit(Interloper& itl, Function& func, implicit_type type)
{
    const auto opcode = Opcode(Implicit {type});
    emit_block_func(itl,func,opcode);
}

Opcode make_lowered_implicit(implicit_type type)
{
    auto opcode = Opcode {Implicit {type}};
    opcode.lowered = true;

    return opcode;
}

void syscall(Interloper& itl, Function& func)
{
    func.leaf_func = false;
    emit_implicit(itl,func,implicit_type::syscall);
}

void ret(Interloper& itl, Function& func)
{
    add_func_exit(func,cur_block(func));
    emit_implicit(itl,func,implicit_type::ret);
}
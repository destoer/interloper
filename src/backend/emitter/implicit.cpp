
void emit_implicit(Interloper& itl, Function& func, implicit_type type)
{
    Opcode opcode;
    opcode.group = op_group::implicit;
    opcode.implicit = {type};

    emit_block_func(itl,func,opcode);
}

void syscall(Interloper& itl, Function& func)
{
    emit_implicit(itl,func,implicit_type::syscall);
}
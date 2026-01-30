
void emit_branch_label(Interloper& itl, Function& func, branch_label_type type, LabelSlot label)
{   
    UNUSED(itl);
    Opcode opcode;
    opcode.group = op_group::branch_label;
    opcode.branch_label = {type,label};

    emit_block_func(func,opcode);
}

void call(Interloper& itl, Function& func, LabelSlot label)
{
    emit_branch_label(itl,func,branch_label_type::call,label);
}
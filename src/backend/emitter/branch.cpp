ConstRegSpan branch_reg_span(const BranchReg&  branch, RegSpan& span)
{
    span.src[0] = branch.src.ir;
    span.src.size = 1;
    span.dst.size = 0;

    return span;
}

void emit_branch_label(Interloper& itl, Function& func, branch_type type, LabelSlot label)
{   
    Opcode opcode;
    opcode.group = op_group::branch_label;
    opcode.branch_label = {type,label};

    emit_block_func(itl,func,opcode);
}

void emit_branch_reg(Interloper& itl, Function& func, branch_type type, RegSlot src)
{   
    Opcode opcode;
    opcode.group = op_group::branch_reg;
    BranchReg branch_reg;
    branch_reg.type = type;
    branch_reg.src.ir = src;

    opcode.branch_reg = branch_reg;

    emit_block_func(itl,func,opcode);
}

void call(Interloper& itl, Function& func, LabelSlot label)
{
    emit_branch_label(itl,func,branch_type::call,label);
}

void call_reg(Interloper& itl, Function& func, RegSlot src)
{
    emit_branch_reg(itl,func,branch_type::call,src);
}
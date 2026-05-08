ConstIrRegSpan branch_reg_span(const BranchReg&  branch, IrRegSpan& span)
{
    span.src[0] = branch.src.ir;
    span.src.size = 1;

    return span;
}

ConstIrRegSpan branch_cond_reg_span(const BranchCond&  branch, IrRegSpan& span)
{
    span.src[0] = branch.src.ir;
    span.src.size = 1;
    return span;
}

Opcode make_branch_cond(RegSlot src, LabelSlot label, branch_cond_type type)
{   
    BranchCond cond;
    cond.type = type;
    cond.src.ir = src;
    cond.label = label;


    return Opcode(cond);
}

Opcode make_branch_label(branch_type type, LabelSlot label)
{   
    return Opcode(BranchLabel {type,label});
}

void emit_branch_reg(Interloper& itl, Function& func, branch_type type, RegSlot src)
{   
    BranchReg branch_reg;
    branch_reg.type = type;
    branch_reg.src.ir = src;
    
    const auto opcode = Opcode(branch_reg);
    emit_block_func(itl,func,opcode);
}

void call(Interloper& itl, Function& func, LabelSlot label)
{
    func.leaf_func = false;
    spill_func_bounds(itl,func);

    const auto branch = make_branch_label(branch_type::call,label);
    emit_block_func(itl,func,branch);
}

void call_reg(Interloper& itl, Function& func, RegSlot src)
{
    func.leaf_func = false;
    spill_func_bounds(itl,func);

    emit_branch_reg(itl,func,branch_type::call,src);
}

void branch_reg(Interloper& itl, Function& func, RegSlot src)
{
    emit_branch_reg(itl,func,branch_type::branch,src);
}
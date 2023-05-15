
/*
    void add_exit(Function& func,u32 block_slot, u32 exit)
    {
        auto &block = func.emitter.program[block_slot];

        push_var(block.exit,exit);
    }

    void cond_branch_exit(Function& func, u32 block_slot, u32 target, u32 fallthrough)
    {
        add_exit(func,block_slot,target);
        add_exit(func,block_slot,fallthrough);
    }
*/

/*
void add_exit(Function& func, BlockSlot block, LabelSlot exit)
{

}
*/

void emit_call(Function& func, LabelSlot label, b32 save_regs)
{
    if(save_regs)
    {
        // Handle saving any caller saved regs
        emit(func,op_type::spill_rv);
    }
    
    emit_internal(func,op_type::call, label.handle,0,0);
}

void emit_cond_branch(Function& func, BlockSlot block,LabelSlot label, SymSlot sym, b32 cond)
{
    const op_type type = cond? op_type::bc : op_type::bnc;

    emit_block_internal(func,block,type,label.handle,sym.handle,0);

}
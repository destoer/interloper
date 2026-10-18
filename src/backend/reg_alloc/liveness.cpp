
struct VariableRange
{
    u32 start = 0xffff'ffff;
    u32 end = 0;
    LocalSlot slot = {INVALID_HANDLE};
    u32 global_reg = REG_FREE;
    BlockSlot block_slot = {INVALID_HANDLE};
};


struct VariableLive
{
    Array<VariableRange> range;
};

struct ProgramRange
{
    Array<VariableLive> var;
};

enum class usage_type
{
    use,
    def
};

ProgramRange make_linear_program(Function& func)
{
    ProgramRange program;

    resize(program.var,count(func.local.registers));

    return program;
}

void number_cfg(Function& func)
{
    u32 pc = 0;

    // Add in increments of two so we can insert 
    // spills/loads on odd addresses.
    for(auto& block : func.emitter.program)
    {
        block.pc_start = pc;
        pc += 2;

        for(auto& node : block.list)
        {
            node.value.pc = pc;
            pc += 2;
        }

        block.pc_end = pc;
    }
}

void collapse_ranges(VariableLive& variable)
{
    // TODO: There will be more efficient ways to do this
    for(u32 r = 0; r < count(variable.range) - 1; r++)
    {
        auto& cur = variable.range[r];
        auto& next = variable.range[r + 1];

        if(in_range(cur.start,next.start,next.end) || in_range(next.start,cur.start,cur.end))
        {
            cur.end = max(cur.end,next.end);
            cur.start = min(cur.start,next.start);
            remove_out_of_place(variable.range,r+1);
        }
    }
}

void update_range(ProgramRange& program, LocalSlot slot, Block& block,u32 pc)
{
    auto& variable = program.var[slot.handle];

    const b32 def = contains(block.def,slot);

    // If this is the first range or we have just hit a def then we need to create a new range
    if(!variable.range || (def && variable.range[count(variable.range) - 1].block_slot != block.block_slot))
    {
        resize_end(variable.range,1);
    }

    auto& range = variable.range[count(variable.range) - 1];
    range.slot = slot;

    if(pc < range.start)
    {
        range.block_slot = block.block_slot;
        range.start = pc;
    }

    range.end = std::max(range.end,pc);
    collapse_ranges(variable);
}

void update_regs_range(ProgramRange& program, Block& block, u64 pc, const ConstSpan<RegSlot>& regs)
{
    for(const auto& reg : regs)
    {
        if(reg.kind != reg_kind::local)
        {
            continue;
        }

        update_range(program,reg.local,block,pc);
    }
}


ProgramRange find_program_ranges(Interloper& itl, Function& func)
{
    ProgramRange program = make_linear_program(func);

    // First we need a flat view of the program
    number_cfg(func);

    // for each block
    for(auto& block : func.emitter.program)
    {
        OpcodeNode* node = block.list.start;

        for(const LocalSlot slot : block.live_in)
        {
            update_range(program,slot,block,block.pc_start);
        }

        // for each opcode
        while(node)
        {
            const auto& opcode = node->value;

            const auto regs = opcode_ir_reg_span(opcode,itl.reg_span);

            update_regs_range(program,block,opcode.pc,regs.src);
            update_regs_range(program,block,opcode.pc,regs.dst_src);

            // dst only regs have have a later pc
            update_regs_range(program,block,opcode.pc + 1,regs.dst);


            // next opcode
            node = node->next;
        }
        

        // if its live out then consider it used in every exit of use
        // TODO: We may want to optimize this with a bit set and?
        for(const LocalSlot slot : block.live_out)
        {
            for(const BlockSlot exit: block.exit)
            {
                auto& exit_block = block_from_slot(func,exit);
                if(contains(exit_block.use,slot))
                {
                    update_range(program,slot,block,exit_block.pc_start);
                }
            }
        }
    }

    return program;
}
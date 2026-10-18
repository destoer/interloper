
struct VariableRange
{
    u32 start = 0xffff'ffff;
    u32 end = 0;
    BlockSlot block_slot = {INVALID_HANDLE};
};

// TODO: Probably want a complete range for optimization reasons.
struct VariableLive
{
    LocalSlot slot = {INVALID_HANDLE};
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
    for(u32 v = 0; v < count(program.var); v++)
    {
        program.var[v].slot = {v};
    }

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

bool variable_range_overlaps(const VariableRange& v1, const VariableRange& v2)
{
    return in_range(v1.start,v2.start,v2.end) || in_range(v2.start,v1.start,v2.end);
}

bool variable_ranges_overlap(const VariableLive& var, const VariableLive& other)
{
    for(const auto& range : var.range)
    {
        for(const auto& other_range: other.range)
        {
            if(variable_range_overlaps(range,other_range))
            {
                return true;
            }
        }   
    }

    return false;
}

void collapse_ranges(VariableLive& variable)
{
    // TODO: There will be more efficient ways to do this
    for(u32 r = 0; r < count(variable.range) - 1; r++)
    {
        auto& cur = variable.range[r];
        auto& next = variable.range[r + 1];

        if(variable_range_overlaps(cur,next))
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
        for(const LocalSlot slot : block.live_in)
        {
            update_range(program,slot,block,block.pc_start);
        }

        // for each opcode
        for(const OpcodeNode& node : block.list)
        {
            const auto& opcode = node.value;

            const auto regs = opcode_ir_reg_span(opcode,itl.reg_span);

            update_regs_range(program,block,opcode.pc,regs.src);
            update_regs_range(program,block,opcode.pc,regs.dst_src);

            // dst only regs have have a later pc
            update_regs_range(program,block,opcode.pc + 1,regs.dst);
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
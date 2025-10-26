#include <interloper.h>
#include "backend/op_table.inl"

#include "backend/pool.cpp"
#include "backend/emitter.cpp"
#include "backend/rewrite_arch_ir.cpp"
#include "backend/cfg.cpp"
#include "backend/asm.cpp"
#include "backend/x86_emitter.cpp"
#include "backend/stack_allocator.cpp"
#include "backend/linear_alloc.cpp"
#include "backend/disass.cpp"
#include "backend/ir_x86.cpp"
#include "backend/elf.cpp"
#include "backend/intrin.cpp"
#include "backend/ir.cpp"


Option<itl_error> func_graph_pass(Interloper& itl, Function& func)
{
    // if final block has no return and this is a void func insert one
    if(is_void(func.sig.return_type[0]))
    {    
        auto& end_block = block_from_slot(func,cur_block(func));

        if(!has_func_exit(func,end_block.block_slot))
        {
            ret(itl,func);
        }
    }

    // connect up the cfg
    connect_flow_graph(itl,func); 

    // do liveness analysis
    compute_var_live(itl,func);


    // now check a function exit is reachable from the entry block of the function
    // for a void func this should allways be possible as everything should hit the bottom return
    // that does not have an early return...

    auto& start_block = func.emitter.program[0];

    // check the start block can reach one
    if(!can_reach_exit(start_block))
    {
        auto& label = label_from_slot(itl.symbol_table.label_lookup,start_block.label_slot);

        itl.ctx.expr = (AstNode*)func.root;
        return compile_error(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %s",label.name.buf); 
    }

    for(BlockSlot slot : start_block.links)
    {
        // TODO: have this print the source line of the block
        if(!can_reach_exit(func,slot))
        {
            auto& block = block_from_slot(func,slot);
            auto& label = label_from_slot(itl.symbol_table.label_lookup,block.label_slot);

            itl.ctx.expr = (AstNode*)func.root;   
            dump_ir_sym(itl,func,itl.symbol_table);
            return compile_error(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %s",label.name.buf);
        }
    }

    return option::none;
}

void setup_passing_convention(Interloper& itl, Function& func)
{
    lock_reg_set(itl,func,func.sig.locked_set);

    // Setup calling convention
    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        const SymSlot slot = func.sig.args[a];
        const u32 arg_reg = func.sig.pass_as_reg[a];

        if(arg_reg != NON_ARG)
        {
            const spec_reg arg = spec_reg(SPECIAL_REG_ARG_START + arg_reg);
            mov_unlock(itl,func,make_sym_reg_slot(slot),make_spec_reg_slot(arg));
        }
    }
}

void compile_block(Interloper& itl, Function& func,AstBlock& block)
{
    UNUSED(itl); UNUSED(func); UNUSED(block);
    assert(false);
}

Option<itl_error> compile_function(Interloper& itl, Function& func)
{
    // Dummy function just output a block and setup the graph
    if(!func.root)
    {
        // produce a dummy basic block
        if(count(func.emitter.program) == 0)
        {
            new_basic_block(itl,func);
        }

        return func_graph_pass(itl,func);
    }

    new_basic_block(itl,func);
    setup_passing_convention(itl,func);

    compile_block(itl,func,func.root->block);
    return func_graph_pass(itl,func);
}

Option<itl_error> compile_functions(Interloper& itl)
{
    for(Function* func : itl.func_table.used)
    {
        const auto func_err = compile_function(itl,*func);
        if(!func_err)
        {
            return func_err;
        }
    }

    return option::none;
}

Option<itl_error> backend(Interloper& itl)
{
    const auto func_err = compile_functions(itl);
    if(func_err)
    {
        return func_err;
    }

    UNUSED(itl);
    assert(false);   
}
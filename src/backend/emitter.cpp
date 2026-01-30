#include "opcode.h"

void handle_src_storage(Interloper& itl, Function& func, RegSlot src)
{
    if(is_special_reg(src))
    {
        return;
    }

    auto& reg = reg_from_slot(itl.symbol_table,func,src);

    if(is_aliased(reg))
    {
        reload_slot(itl,func,reg);
    }
}


// NOTE: these are the bottom level emitter only use directly if you need to gen code yourself
OpcodeNode* emit_block_internal(Function& func,BlockSlot block_slot, const Opcode& opcode)
{
    auto& block = block_from_slot(func,block_slot);

    auto &list = block.list;
    append(list,opcode);
    
    return list.finish;    
}

OpcodeNode* emit_block_func(Function& func,const Opcode& opcode)
{
    return emit_block_internal(func,cur_block(func),opcode);
}

void emit_directive(Interloper& itl, Function& func, const Directive& directive)
{
    Opcode opcode;
    opcode.type = op_group::directive;
    opcode.directive = directive;

    emit_block_func(itl,func,opcode);
}

void push_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    handle_src_storage(itl,func,src);

    Directive directive;
    directive.type = directive_type::push_arg;
    directive.v[0] = make_reg_operand(src);

    emit_directive(itl,func,directive);
}
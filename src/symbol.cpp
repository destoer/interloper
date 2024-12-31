#include <interloper.h>
#include "type.cpp"

void enter_new_anon_scope(SymbolTable& sym_table)
{
    sym_table.cur_namespace = new_anon_scope(sym_table.cur_namespace);
}

void destroy_scope(SymbolTable &sym_table)
{
    sym_table.cur_namespace = sym_table.cur_namespace->parent;
}

u32 sym_to_idx(SymSlot s)
{
    return s.handle - SYMBOL_START;
}


// NOTE: reference may move, hold the slot if needed for extended periods
Symbol& sym_from_slot(SymbolTable &table, SymSlot slot)
{
    return table.slot_lookup[sym_to_idx(slot)]; 
}

const Symbol& sym_from_slot(const SymbolTable &table, SymSlot slot)
{
    return table.slot_lookup[sym_to_idx(slot)]; 
}

Reg& reg_from_slot(SymbolTable &table,Array<Reg> &tmp_regs, SymSlot slot)
{
    if(is_special_reg(slot))
    {
        assert(false);
    }

    if(!is_tmp(slot))
    {
        return sym_from_slot(table,slot).reg;
    }

    else
    {
        return tmp_regs[slot.handle];
    }
}

Reg& reg_from_slot(SymbolTable &table,Function& func, SymSlot slot)
{
    return reg_from_slot(table,func.registers,slot);
}

Reg& reg_from_slot(Interloper& itl,Function& func, SymSlot slot)
{
    return reg_from_slot(itl.symbol_table,func,slot);
}

Symbol* get_sym(SymbolTable &sym_table,const String &sym)
{
    const DefInfo* def_info = lookup_definition(sym_table.cur_namespace,sym);

    if(def_info && def_info->type == definition_type::variable)
    {
        const auto slot = sym_from_idx(def_info->handle);
        return &sym_from_slot(sym_table,slot);
    }

    return nullptr;
}

b32 symbol_exists(SymbolTable &sym_table,const String &sym)
{
    return get_sym(sym_table,sym) != nullptr;
}


Symbol make_sym(Interloper& itl,const String& name, Type* type,u32 arg = NON_ARG)
{
    auto& table = itl.symbol_table;

    const u32 slot = symbol(count(table.slot_lookup));

    Symbol symbol = {};
    symbol.name = copy_string(*table.string_allocator,name);
    symbol.type = type;
    symbol.arg_offset = arg;
    symbol.scope_end = block_from_idx(0xffff'ffff);

    symbol.reg = make_reg(itl,reg_kind::local,slot,type);

    // mark an offset so it is not unallocated
    if(symbol.arg_offset != NON_ARG)
    {
        symbol.reg.flags |= FUNC_ARG;
        symbol.reg.offset = 0;
    }

    return symbol;
}




// add symbol to slot lookup
void add_var(SymbolTable &sym_table,Symbol &sym)
{
    push_var(sym_table.slot_lookup,sym);    
}

// add symbol to the scope table
void add_sym_to_scope(SymbolTable &sym_table, Symbol &sym)
{
    const DefInfo info = {definition_type::variable,sym.reg.slot.handle};
    add(sym_table.cur_namespace->table,sym.name, info);
}    

Symbol &add_symbol(Interloper &itl,const String &name, Type *type)
{
    auto& sym_table = itl.symbol_table;

    auto sym = make_sym(itl,name,type);
    push_var(sym_table.slot_lookup,sym);

    add_sym_to_scope(sym_table,sym);

    return sym_from_slot(sym_table,sym.reg.slot);
}

Symbol& add_global(Interloper& itl,const String &name, Type *type, b32 constant)
{
    auto& sym_table = itl.symbol_table;

    auto sym = make_sym(itl,name,type);
    sym.reg.kind = constant? reg_kind::constant : reg_kind::global;

    push_var(sym_table.slot_lookup,sym);

    if(!constant)
    {
        push_var(sym_table.global,sym.reg.slot);
    }

    // add this into the top level scope
    const DefInfo info = {definition_type::variable,sym.reg.slot.handle};
    add(itl.global_namespace->table,sym.name, info);    

    return sym_from_slot(sym_table,sym.reg.slot);
}

LabelSlot label_from_idx(u32 handle)
{
    LabelSlot slot;
    slot.handle = handle;

    return slot;
}

LabelSlot add_label(SymbolTable &sym_table,const String &name)
{
    Label label;
    label.name = copy_string(*sym_table.string_allocator,name);
    label.offset = 0;

    const u32 handle = count(sym_table.label_lookup);

    push_var(sym_table.label_lookup,label);

    return label_from_idx(handle);
}

void destroy_sym_table(SymbolTable &sym_table)
{
    // TODO: Destroy the scoping tree

    for(u32 s = 0; s < count(sym_table.slot_lookup); s++)
    {
        auto& sym = sym_table.slot_lookup[s];
        destroy_reg(sym.reg);
    }

    destroy_arr(sym_table.slot_lookup);
    destroy_arr(sym_table.label_lookup);
    destroy_arr(sym_table.global);
}


bool is_arg(const Symbol &sym)
{
    return sym.arg_offset != NON_ARG;
}

void print(Interloper& itl,const Symbol& sym)
{
    printf("name: %s\n",sym.name.buf);
    printf("type: %s\n",type_name(itl,sym.type).buf);
    printf("arg_offset: %x\n",sym.arg_offset);
    print(sym.reg);
}

void dump_slots(Interloper& itl,SlotLookup &slot_lookup)
{
    for(u32 i = 0; i < count(slot_lookup); i++)
    {
        print(itl,slot_lookup[i]);
    }
}

Label& label_from_slot(LabelLookup& lookup, LabelSlot slot)
{
    return lookup[slot.handle];
}

const Label& label_from_slot(const LabelLookup& lookup, LabelSlot slot)
{
    return lookup[slot.handle];
}


String alloc_name_space_name(ArenaAllocator& allocator,const String& name_space, const String& name)
{
    // just as is
    if(name_space == "")
    {
        return copy_string(allocator,name);
    }

    // name_space + "::" + name;
    StringBuffer buffer;

    push_string(allocator,buffer,name_space);
    push_string(allocator,buffer,"::");
    push_string(allocator,buffer,name);

    // null term the buffer
    push_char(allocator,buffer,'\0');

    return make_string(buffer);
}
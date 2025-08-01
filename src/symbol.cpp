#include <interloper.h>
#include "type.cpp"

SymbolScopeGuard enter_new_anon_scope(SymbolTable& sym_table)
{
    sym_table.ctx->name_space = new_anon_scope(*sym_table.namespace_allocator,sym_table.ctx->name_space);
    return SymbolScopeGuard(sym_table);
}

void destroy_scope(SymbolTable &sym_table)
{
    if(sym_table.ctx->name_space)
    {
        sym_table.ctx->name_space = sym_table.ctx->name_space->parent;
    }
}

SymSlot slot_from_sym(const Symbol& sym)
{
    return sym.reg.slot.sym_slot;
}

u32 handle_from_sym(const Symbol& sym)
{
    return slot_from_sym(sym).handle;
}

// NOTE: reference may move, hold the slot if needed for extended periods
Symbol& sym_from_slot(SymbolTable &table, SymSlot slot)
{
    return table.slot_lookup[slot.handle]; 
}

const Symbol& sym_from_slot(const SymbolTable &table, SymSlot slot)
{
    return table.slot_lookup[slot.handle]; 
}

Reg& reg_from_slot(SymbolTable &table,Array<Reg> &tmp_regs, const RegSlot& slot)
{
    switch(slot.kind)
    {
        case reg_kind::tmp:
        {
            return tmp_regs[slot.tmp_slot.handle];
        }

        case reg_kind::sym:
        {
            return sym_from_slot(table,slot.sym_slot).reg;
        }

        // These don't have registers backing them
        case reg_kind::spec:
        {
            assert(false);
            break;
        }
    }

    assert(false);
}

Reg& reg_from_slot(SymbolTable &table,Function& func, const RegSlot& slot)
{
    return reg_from_slot(table,func.registers,slot);
}

Reg& reg_from_slot(Interloper& itl,Function& func, const RegSlot& slot)
{
    return reg_from_slot(itl.symbol_table,func,slot);
}

Symbol* get_sym_internal(SymbolTable &sym_table,const String &sym, NameSpace* name_space)
{
    const DefInfo* def_info = lookup_typed_definition(name_space? name_space : sym_table.ctx->name_space,sym,definition_type::variable);

    if(def_info)
    {
        const auto slot = sym_from_idx(def_info->handle);
        return &sym_from_slot(sym_table,slot);
    }

    return nullptr;
}

Symbol* get_sym(SymbolTable &sym_table,const String &sym)
{
    return get_sym_internal(sym_table,sym,nullptr);
}

Symbol* get_sym_scoped(SymbolTable &sym_table,const String &sym, NameSpace* name_space)
{
    return get_sym_internal(sym_table,sym,name_space);
}


b32 symbol_exists(SymbolTable &sym_table,const String &sym)
{
    return get_sym(sym_table,sym) != nullptr;
}


Symbol make_sym(Interloper& itl,const String& name, Type* type,u32 arg = NON_ARG)
{
    auto& table = itl.symbol_table;

    const SymSlot sym_slot = {count(table.slot_lookup)};

    Symbol symbol = {};
    symbol.name = copy_string(*table.string_allocator,name);
    symbol.type = type;
    symbol.arg_offset = arg;
    symbol.scope_end = block_from_idx(0xffff'ffff);

    const auto reg_slot = make_sym_reg_slot(sym_slot);

    symbol.reg = make_reg(itl,reg_slot,type);

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
    const DefInfo info = {definition_type::variable,handle_from_sym(sym)};
    add(sym_table.ctx->name_space->table,sym.name, info);
}    

Symbol &add_symbol(Interloper &itl,const String &name, Type *type)
{
    auto& sym_table = itl.symbol_table;

    auto sym = make_sym(itl,name,type);
    push_var(sym_table.slot_lookup,sym);

    add_sym_to_scope(sym_table,sym);

    return sym_from_slot(sym_table,slot_from_sym(sym));
}

Symbol& add_global(Interloper& itl,const String &name, Type *type, b32 constant)
{
    auto& sym_table = itl.symbol_table;

    auto sym = make_sym(itl,name,type);
    sym.reg.segment = constant? reg_segment::constant : reg_segment::global;

    push_var(sym_table.slot_lookup,sym);

    const auto slot = slot_from_sym(sym);

    if(!constant)
    {
        push_var(sym_table.global,slot);
    }

    // add this into the top level scope
    const DefInfo info = {definition_type::variable,handle_from_sym(sym)};
    add(itl.global_namespace->table,sym.name, info);    

    return sym_from_slot(sym_table,slot);
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

TypedReg typed_reg(const Symbol& sym)
{
    return TypedReg{sym.reg.slot,sym.type};
}

TypedAddr typed_addr(const Symbol& sym)
{
    // A fixed array is not a real struct so we have to lie.
    if(is_fixed_array(sym.type))
    {
        return TypedAddr(make_addr(sym.reg.slot,0),sym.type);
    }

    return TypedAddr{make_struct_addr(sym.reg.slot,0),sym.type};
}
#include <interloper.h>
#include "type.cpp"


void new_scope(SymbolTable &sym_table)
{
    push_var<HashTable<String,SymSlot>,HashTable<String,SymSlot>>(sym_table.table,make_table<String,SymSlot>());
}

void destroy_scope(SymbolTable &sym_table)
{
    sym_table.sym_count -= sym_table.table[count(sym_table.table) - 1].size;
    auto table = pop(sym_table.table);
    destroy_table(table);
}

u32 sym_to_idx(SymSlot s)
{
    return s.handle - SYMBOL_START;
}


Symbol& sym_from_slot(SymbolTable &table, SymSlot slot)
{
    return table.slot_lookup[sym_to_idx(slot)]; 
}

const Symbol& sym_from_slot(const SymbolTable &table, SymSlot slot)
{
    return table.slot_lookup[sym_to_idx(slot)]; 
}

Reg& reg_from_slot(SymbolTable &table,Function& func, SymSlot slot)
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
        return func.registers[slot.handle];
    }
}


Symbol* get_sym(SymbolTable &sym_table,const String &sym)
{
    for(s32 i = count(sym_table.table) - 1; i >= 0; i--)
    {
        const SymSlot* slot = lookup(sym_table.table[i],sym);

        if(slot)
        {
            return &sym_from_slot(sym_table,*slot);
        }
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

    return symbol;
}




// add symbol to slot lookup
void add_var(SymbolTable &sym_table,Symbol &sym)
{
    push_var(sym_table.slot_lookup,sym);    
}

// add symbol to the scope table
void add_scope(SymbolTable &sym_table, Symbol &sym)
{
    add(sym_table.table[count(sym_table.table) - 1],sym.name, sym.reg.slot);
    sym_table.sym_count++;
}    

Symbol &add_symbol(Interloper &itl,const String &name, Type *type)
{
    auto& sym_table = itl.symbol_table;

    auto sym = make_sym(itl,name,type);
    push_var(sym_table.slot_lookup,sym);

    add_scope(sym_table,sym);

    return sym_from_slot(sym_table,sym.reg.slot);
}

Symbol& add_global(Interloper& itl,const String &name, Type *type)
{
    Symbol& sym = add_symbol(itl,name,type);
    sym.reg.location = LOCATION_GLOBAL;

    return sym;
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
    for(u32 h = 0; h < count(sym_table.table); h++)
    {
        destroy_table(sym_table.table[h]);
    }

    destroy_arr(sym_table.table);

    for(u32 s = 0; s < count(sym_table.slot_lookup); s++)
    {
        auto& sym = sym_table.slot_lookup[s];
        destroy_reg(sym.reg);
    }

    destroy_arr(sym_table.slot_lookup);
    destroy_arr(sym_table.label_lookup);

    sym_table.sym_count = 0;
    sym_table.var_count = 0;
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
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



std::optional<Symbol> get_sym(SymbolTable &sym_table,const String &sym)
{
    for(s32 i = count(sym_table.table) - 1; i >= 0; i--)
    {
        const SymSlot* slot = lookup(sym_table.table[i],sym);

        if(slot)
        {
            auto sym = sym_from_slot(sym_table,*slot);
            return std::optional(sym);
        }
    }

    return std::nullopt;
}


Symbol make_sym(SymbolTable& table,const String& name, Type* type, u32 size,u32 arg = NON_ARG)
{
    const u32 slot = symbol(count(table.slot_lookup));

    Symbol symbol = {};
    symbol.name = copy_string(*table.string_allocator,name);
    symbol.type = type;
    symbol.arg_offset = arg;
    symbol.scope_end = block_from_idx(0xffff'ffff);

    b32 s = is_signed(type);

    symbol.reg = make_reg(reg_kind::local,size,slot,s);

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

Symbol &add_symbol(SymbolTable &sym_table,const String &name, Type *type, u32 size)
{
    auto sym = make_sym(sym_table,name,type,size);
    push_var(sym_table.slot_lookup,sym);

    add_scope(sym_table,sym);

    return sym_from_slot(sym_table,sym.reg.slot);
}

Symbol& add_global(SymbolTable &sym_table,const String &name, Type *type, u32 size)
{
    Symbol& sym = add_symbol(sym_table,name,type,size);
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

void clear(SymbolTable &sym_table)
{
    for(u32 h = 0; h < count(sym_table.table); h++)
    {
        destroy_table(sym_table.table[h]);
    }

    destroy_arr(sym_table.table);
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
#include <interloper.h>
#include "type.cpp"


void new_scope(SymbolTable &sym_table)
{
    push_var<HashTable<u32>,HashTable<u32>>(sym_table.table,make_table<u32>());
}

void destroy_scope(SymbolTable &sym_table)
{
    sym_table.sym_count -= sym_table.table[count(sym_table.table) - 1].size;
    auto table = pop(sym_table.table);
    destroy_table(table);
}

u32 sym_to_idx(u32 s)
{
    return s - SYMBOL_START;
}


Symbol& sym_from_slot(SlotLookup &slot_lookup, u32 slot)
{
    return slot_lookup[sym_to_idx(slot)]; 
}


std::optional<Symbol> get_sym(SymbolTable &sym_table,const String &sym)
{
    for(s32 i = count(sym_table.table) - 1; i >= 0; i--)
    {
        const u32* idx = lookup(sym_table.table[i],sym);

        if(idx)
        {
            return std::optional<Symbol>(sym_table.slot_lookup[*idx]);
        }
    }

    return std::nullopt;
}


Symbol make_sym(SymbolTable& table,const String& name, const Type& type, u32 size, u32 arg = NON_ARG)
{
    Symbol symbol = {};
    symbol.name = copy_string(*table.string_allocator,name);
    symbol.type = type;
    symbol.size = size;
    symbol.arg_offset = arg;

    return symbol;
}

Symbol make_sym(SymbolTable& table,const char* name, const Type& type, u32 size, u32 arg = NON_ARG)
{
    Symbol symbol = {};
    symbol.name = make_string(*table.string_allocator,name,strlen(name));
    symbol.type = type;
    symbol.size = size;
    symbol.arg_offset = arg;

    return symbol;
}



// add symbol to slot lookup
void add_var(SymbolTable &sym_table,Symbol &sym)
{
    sym.slot = symbol(sym_table.slot_lookup.size());
    sym_table.slot_lookup.push_back(sym);    
}

// add symbol to the scope table
void add_scope(SymbolTable &sym_table, Symbol &sym)
{
    add(sym_table.table[count(sym_table.table) - 1],sym.name, sym_to_idx(sym.slot));
    sym_table.sym_count++;
}    

Symbol &add_symbol(SymbolTable &sym_table,const String &name, const Type &type, u32 size)
{
    auto sym = make_sym(sym_table,name,type,size);

    sym.slot = symbol(sym_table.slot_lookup.size());
    sym_table.slot_lookup.push_back(sym);  

    add_scope(sym_table,sym);

    return sym_from_slot(sym_table.slot_lookup,sym.slot);
}

void add_label(SymbolTable &sym_table,const String &name)
{
    Label label;
    label.name = copy_string(*sym_table.string_allocator,name);
    label.offset = 0;

    sym_table.label_lookup.push_back(label);
}

void clear(SymbolTable &sym_table)
{
    for(u32 h = 0; h < count(sym_table.table); h++)
    {
        destroy_table(sym_table.table[h]);
    }

    destroy_arr(sym_table.table);

    sym_table.label_lookup.clear();
    sym_table.slot_lookup.clear();
    sym_table.sym_count = 0;
    sym_table.var_count = 0;
}


bool is_arg(const Symbol &sym)
{
    return sym.arg_offset != NON_ARG;
}

void print(Interloper& itl,const Symbol&sym)
{
    printf("name: %s\n",sym.name.buf);
    printf("type: %s\n",type_name(itl,sym.type).c_str());
    printf("slot: %x\n",sym.slot);
    printf("arg_offset: %x\n",sym.arg_offset);
    printf("offset: %x\n",sym.offset);
    printf("location: %x\n\n",sym.location);
}

void dump_slots(Interloper& itl,SlotLookup &slot_lookup)
{
    for(const auto &sym: slot_lookup)
    {
        print(itl,sym);
    }
}
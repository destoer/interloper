#include <interloper.h>
#include "type.cpp"


void new_scope(SymbolTable &sym_table)
{
    sym_table.table.push_back({});
}

void destroy_scope(SymbolTable &sym_table)
{
    sym_table.sym_count -= sym_table.table.back().size();
    sym_table.table.pop_back();
}

u32 sym_to_idx(u32 s)
{
    return s - SYMBOL_START;
}


Symbol& sym_from_slot(SlotLookup &slot_lookup, u32 slot)
{
    return slot_lookup[sym_to_idx(slot)]; 
}


std::optional<Symbol> get_sym(SymbolTable &sym_table,const std::string &sym)
{
    for(int i = sym_table.table.size()-1; i >= 0; i--)
    {
        if(sym_table.table[i].count(sym))
        {
            const auto idx = sym_table.table[i][sym];
            return std::optional<Symbol>(sym_table.slot_lookup[idx]);
        }
    }

    return std::nullopt;
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
    sym_table.table[sym_table.table.size()-1][sym.name] = sym_to_idx(sym.slot);
    sym_table.sym_count++;
}    

Symbol &add_symbol(SymbolTable &sym_table,const std::string &name, const Type &type, u32 size)
{
    auto sym = Symbol(name,type,size);

    sym.slot = symbol(sym_table.slot_lookup.size());
    sym_table.slot_lookup.push_back(sym);  

    add_scope(sym_table,sym);

    return sym_from_slot(sym_table.slot_lookup,sym.slot);
}

void add_label(SymbolTable &sym_table,const std::string &label)
{
    sym_table.label_lookup.push_back(Label(label,0));
}

void clear(SymbolTable &sym_table)
{
    sym_table.table.clear();
    sym_table.label_lookup.clear();
    sym_table.slot_lookup.clear();
    sym_table.sym_count = 0;
}


bool is_arg(const Symbol &sym)
{
    return sym.arg_offset != NON_ARG;
}

void print(Interloper& itl,const Symbol&sym)
{
    printf("name: %s\n",sym.name.c_str());
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
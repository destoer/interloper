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

std::optional<Symbol> get_sym(SymbolTable &sym_table,const std::string &sym)
{
    for(int i = sym_table.table.size()-1; i >= 0; i--)
    {
        if(sym_table.table[i].count(sym))
        {
            const auto slot = sym_table.table[i][sym];
            return std::optional<Symbol>(sym_table.slot_lookup[slot]);
        }
    }

    return std::nullopt;
}

void print_sym(Interloper& itl,const Symbol &sym)
{
    printf("symbol: %s\n",sym.name.c_str());
    printf("type: %s\n",type_name(itl,sym.type).c_str());
    printf("arg num: %d\n",sym.arg_num);
    printf("slot: %d\n",sym.slot);

    putchar('\n');
}

// add symbol to slot lookup
void add_var(SymbolTable &sym_table,Symbol &sym)
{
    sym.slot = sym_table.slot_lookup.size();
    sym_table.slot_lookup.push_back(sym);    
}

// add symbol to the scope table
void add_scope(SymbolTable &sym_table, Symbol &sym)
{
    sym_table.table[sym_table.table.size()-1][sym.name] = sym.slot;
    sym_table.sym_count++;
}    

Symbol &add_symbol(SymbolTable &sym_table,const std::string &name, const Type &type, u32 size)
{
    auto sym = Symbol(name,type,size);

    sym.slot = sym_table.slot_lookup.size();
    sym_table.slot_lookup.push_back(sym);  

    add_scope(sym_table,sym);

    return sym_table.slot_lookup[sym.slot];
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


u32 slot_idx(const Symbol &sym)
{
    return symbol(sym.slot);
}

bool is_arg(const Symbol &sym)
{
    return sym.arg_num != NON_ARG;
}

void print(Interloper& itl,const Symbol&sym)
{
    printf("name: %s\n",sym.name.c_str());
    printf("type: %s\n",type_name(itl,sym.type).c_str());
    printf("slot: %x\n",sym.slot);
    printf("arg_num: %x\n",sym.arg_num);
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
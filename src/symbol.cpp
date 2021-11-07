#include <interloper.h>

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

void add_slot(SymbolTable &sym_table, Symbol &sym)
{
    sym.slot = sym_table.slot_lookup.size();
    sym_table.slot_lookup.push_back(sym);

    sym_table.table[sym_table.table.size()-1][sym.name] = sym.slot;
    sym_table.sym_count++;
}

void add_symbol(SymbolTable &sym_table,Symbol &symbol)
{
    add_slot(sym_table,symbol);
}

void add_symbol(SymbolTable &sym_table,const std::string &name, const Type &type, u32 size)
{
    // TODO: we need to initialize the size on this!
    auto sym = Symbol(name,type,size);
    add_slot(sym_table,sym);
}

void add_label(SymbolTable &sym_table,const std::string &label)
{
    sym_table.label_lookup.push_back(Label(label,0));
}

void clear(SymbolTable &sym_table)
{
    sym_table.table.clear();
    sym_table.label_lookup.clear();
    sym_table.sym_count = 0;
}


u32 slot_idx(const Symbol &sym)
{
    return sym.arg_num != NON_ARG? arg(sym.slot) : symbol(sym.slot);
}

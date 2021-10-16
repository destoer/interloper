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
            return std::optional<Symbol>(sym_table.table[i][sym]);
        }
    }

    return std::nullopt;
}

void add_symbol(SymbolTable &sym_table,Symbol &symbol, u32 slot)
{
    symbol.slot = slot;
    sym_table.table[sym_table.table.size()-1][symbol.name] = symbol;
    sym_table.sym_count++;
}

void add_symbol(SymbolTable &sym_table,const std::string &name, const Type &type, u32 slot)
{
    sym_table.table[sym_table.table.size()-1][name] = Symbol(name,type,slot);
    sym_table.sym_count++;
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
    return sym.is_arg? arg(sym.slot) : symbol(sym.slot);
}

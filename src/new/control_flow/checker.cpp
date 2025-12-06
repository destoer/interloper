

// TypeResult add_named_symbol(Interloper& itl, NamedSymbol* named_sym, Type* type)
// {
//     const auto sym_ptr = get_sym(itl.symbol_table,named_sym->name);
//     if(!sym_ptr)
//     {
//         return compile_error(itl,itl_error::redeclared,"symbol '%S' is already declared",named_sym->name);
//     }

//     const auto& sym = add_symbol(itl,named_sym,type);

//     named_sym->slot = sym.reg.slot.sym_slot;

//     return sym.type;
// }



// TypeResult type_check_for_range(Interloper& itl, Function& func, AstNode* stmt)
// {
//     UNUSED(func);

//     ForRangeNode* range = (ForRangeNode*)stmt;


// }
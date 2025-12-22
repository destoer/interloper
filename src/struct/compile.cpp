
void compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,AddrSlot addr_slot)
{
    // TODO: add a opt to just memset the entire thing in one go / bulk store
    // NOTE: this should apply all the way down i.e if we contain a struct

    // default construction
    for(const auto& member : structure.members)
    {
        AddrSlot member_addr = addr_slot;
        member_addr.addr.offset += member.offset;

        if(member.expr)
        {
            unimplemented("Member expr default");
            continue;
        }

        switch(member.type->kind)
        {
            // Just recurse this
            case type_class::struct_t:
            {
                const auto nested_structure = struct_from_type(itl.struct_table,member.type);
                compile_struct_decl_default(itl,func,nested_structure,member_addr);
                break;
            }

            case type_class::array_t:
            {
                unimplemented("Default construct arr");
                break;
            }

            // Just zero it.
            default:
            {
                const RegSlot tmp = imm_zero(itl,func);
                const TypedAddr dst_addr = {member_addr,member.type};
                do_addr_store(itl,func,tmp,dst_addr);
                break;
            }
        }
    }
}

void compile_struct_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& sym)
{
    const TypedReg reg = typed_reg(sym);
    alloc_slot(itl,func,reg.slot,true);


    auto structure = struct_from_type(itl.struct_table,reg.type);

    // Default init
    if(!decl_node->expr)
    {
        const AddrSlot addr_slot = make_struct_addr(reg.slot,0);
        compile_struct_decl_default(itl,func,structure,addr_slot);
        return;
    }


    switch(decl_node->expr->type)
    {
        case ast_type::initializer_list:
        {
            unimplemented("Initializer list");
        }

        case ast_type::designated_initializer_list:
        {
            unimplemented("Designated Initializer list");
        }

        case ast_type::no_init:
        {
            return;
        }

        default:
        {
            compile_expression(itl,func,decl_node->expr,reg.slot);
            break;
        }
    }
}

TypedAddr compute_member_addr(Interloper& itl, Function& func, StructAccessNode* struct_access)
{
    TypedAddr struct_addr;

    // parse out initial expr
    switch(struct_access->expr->type)
    {
        case ast_type::symbol:
        {
            SymbolNode* sym_node = (SymbolNode*)struct_access->expr;
            const auto& sym = sym_from_slot(itl.symbol_table,sym_node->sym_slot);

            // allready a pointer so just return the slot
            // along with the derefed type
            if(is_pointer(sym.type))
            {
                struct_addr = {make_pointer_addr(sym.reg.slot,0),deref_pointer(sym.type)};
            }

            else
            {
                // NOTE: For an enum we will us this as a direct index.
                struct_addr = typed_addr(sym);
            }
            break;        
        }

        case ast_type::index:
        {
            unimplemented("Index array struct");
            break;
        }


        default: 
        {
            (void)compile_panic(itl,itl_error::struct_error,"Unknown struct access %s",AST_INFO[u32(struct_access->expr->type)].name);
            break;
        }
    }

    const auto& structure = struct_from_type(itl.struct_table,struct_addr.type);

    // perform each member access
    for(const AccessMember& access_member: struct_access->members)
    {
        // auto deref pointers first
        if(is_pointer(struct_addr.type))
        {
            RegSlot addr_slot = new_tmp_ptr(func);
            do_addr_load(itl,func,addr_slot,struct_addr);

            // now we are back to a straight pointer
            struct_addr = {make_pointer_addr(addr_slot,0),deref_pointer(struct_addr.type)};
        }
        

        switch(access_member.type)
        {
            case member_access_type::struct_t:
            {
                const auto& member = structure.members[access_member.member];

                struct_addr.addr_slot.addr.offset += member.offset;  
                struct_addr.type = member.type;
                break;
            }

            case member_access_type::enum_t:
            {
                unimplemented("Access enum member");
            }

            case member_access_type::array_t:
            {
                unimplemented("Access array member");
            }

            case member_access_type::slice_t:
            {
                unimplemented("Access slice");
            }

            case member_access_type::index_t:
            {
                unimplemented("Access index");
            }
        }
    }

    return struct_addr;
}

void write_struct(Interloper& itl, Function& func, TypedReg src, StructAccessNode* struct_access)
{
    const auto dst_addr = compute_member_addr(itl,func,struct_access);
    do_addr_store(itl,func,src.slot,dst_addr);
}

void read_struct(Interloper& itl, Function& func, StructAccessNode* struct_access, RegSlot dst_slot)
{
    const auto src_addr = compute_member_addr(itl,func,struct_access);
    do_addr_load(itl,func,dst_slot,src_addr);
}

void compile_struct_access(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    read_struct(itl,func,(StructAccessNode*)expr,dst_slot);
}

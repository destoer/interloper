
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
                const auto nested_structure = struct_from_type(itl.struct_table,(StructType*)member.type);
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
                const TypedReg tmp = {imm_zero(itl,func),itl.usize_type};
                const TypedAddr dst_addr = {member_addr,member.type};
                do_addr_store(itl,func,tmp,dst_addr);
                break;
            }
        }
    }
}

void compile_struct_init(Interloper& itl, Function& func, StructType* struct_type,  AddrSlot* addr_slot ,AstNode* expr);

void compile_struct_initializer_list(Interloper& itl, Function& func, Struct& structure, AddrSlot* addr_slot, InitializerListNode* init_list)
{
    for(u32 i = 0; i < count(init_list->list); i++)
    {
        AstNode* node = init_list->list[i];
        auto& member = structure.members[i];

        auto member_addr = *addr_slot;
        member_addr.addr.offset = member.offset;

        switch(member.type->kind)
        {
            // Handle sub array
            case type_class::array_t:
            {
                compile_array_init(itl,func,node,(ArrayType*)member.type,&member_addr);
                break;
            }

            case type_class::struct_t:
            {
                compile_struct_init(itl,func,(StructType*)member.type,&member_addr,node);
                break;
            }

            default:
            {
                auto reg = compile_oper(itl,func,node);

                const TypedAddr dst_addr = {member_addr,member.type};
                do_addr_store(itl,func,reg,dst_addr);
                break;
            }
        }
    }
}

void compile_struct_init(Interloper& itl, Function& func, StructType* struct_type,  AddrSlot* addr_slot ,AstNode* node)
{
    auto& structure = struct_from_type(itl.struct_table,struct_type);

    switch(node->type)
    {
        case ast_type::initializer_list:
        {
            compile_struct_initializer_list(itl,func,structure,addr_slot,(InitializerListNode*)node);
            break;
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
            auto reg = compile_oper(itl,func,node);

            const TypedAddr dst_addr = {*addr_slot,(Type*)struct_type};
            do_addr_store(itl,func,reg,dst_addr);
            break;
        }
    }    
}

void compile_struct_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& sym)
{
    const TypedReg reg = typed_reg(sym);
    alloc_slot(itl,func,reg.slot,true);

    auto struct_type = (StructType*)reg.type;
    auto structure = struct_from_type(itl.struct_table,struct_type);

    // Default init
    if(!decl_node->expr)
    {
        const AddrSlot addr_slot = make_struct_addr(reg.slot,0);
        compile_struct_decl_default(itl,func,structure,addr_slot);
        return;
    }

    AddrSlot addr_slot = make_struct_addr(reg.slot,0);
    compile_struct_init(itl,func,struct_type,&addr_slot,decl_node->expr);
}

void access_array_member(Interloper& itl, TypedAddr* addr, array_member_access member)
{
    switch(member)
    {
        case array_member_access::data:
        {
            ArrayType* array_type = (ArrayType*)addr->type;

            // This is the first member no need to adjust the offset
            addr->type = make_reference(itl,array_type->contained_type);
            break;
        }

        case array_member_access::len:
        {
            // Have to lie about the access because its not actually possible to take a pointer on this.
            if(!is_runtime_size(addr->type))
            {
                addr->addr_slot.addr.base = make_spec_reg_slot(spec_reg::access_fixed_len_reg);
                addr->addr_slot.struct_addr = false;
            }

            // vla
            else
            {
                addr->type = itl.usize_type;
                addr->addr_slot.addr.offset += GPR_SIZE;
            }

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
            struct_addr = index_arr(itl,func,(IndexNode*)struct_access->expr);
            break;
        }


        default: 
        {
            (void)compile_panic(itl,itl_error::struct_error,"Unknown struct access %s",AST_INFO[u32(struct_access->expr->type)].name);
            break;
        }
    }

    const auto& structure = struct_from_type(itl.struct_table,(StructType*)struct_addr.type);

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
                const auto array_member = array_member_access(access_member.member);
                access_array_member(itl,&struct_addr,array_member);
                break;
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
    do_addr_store(itl,func,src,dst_addr);
}

void read_struct(Interloper& itl, Function& func, StructAccessNode* struct_access, RegSlot dst_slot)
{
    const auto src_addr = compute_member_addr(itl,func,struct_access);

    // len access on fixed sized array
    if(is_special_reg(src_addr.addr_slot.addr.base,spec_reg::access_fixed_len_reg))
    {
        const ArrayType* array_type = (ArrayType*)src_addr.type;
        mov_imm(itl,func,dst_slot,array_type->size);
        return;
    }    

    do_addr_load(itl,func,dst_slot,src_addr);
}

void compile_struct_access(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    read_struct(itl,func,(StructAccessNode*)expr,dst_slot);
}

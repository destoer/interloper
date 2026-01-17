
#include "destoer/destoer.h"
#include "interloper.h"
#include "parser.h"
#include "sym.h"
#include "type.h"


void compile_struct_init(Interloper& itl, Function& func,AstNode* expr, StructType* struct_type,  AddrSlot* addr_slot);
void compile_struct_initializer_item(Interloper& itl, Function& func, const AddrSlot& addr_slot, const Member& member, AstNode* node);

void compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,AddrSlot addr_slot)
{
    // TODO: add a opt to just memset the entire thing in one go / bulk store
    // NOTE: this should apply all the way down i.e if we contain a struct

    // default construction
    for(const auto& member : structure.members)
    {
        if(member.expr)
        {
            compile_struct_initializer_item(itl,func,addr_slot,member,member.expr);
            continue;
        }

        AddrSlot member_addr = addr_slot;
        member_addr.addr.offset += member.offset;

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
                default_construct_array(itl, func, (ArrayType*)member.type, member_addr);
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

void compile_struct_initializer_item(Interloper& itl, Function& func, const AddrSlot& addr_slot, const Member& member, AstNode* node)
{
    if(node->type == ast_type::no_init)
    {
        return;
    }

    const u32 base = addr_slot.addr.offset;

    auto member_addr = addr_slot;
    member_addr.addr.offset = base + member.offset;

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
            compile_struct_init(itl,func,node,(StructType*)member.type,&member_addr);
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

void compile_struct_initializer_list(Interloper& itl, Function& func, Struct& structure, AddrSlot addr_slot, InitializerListNode* init_list)
{
    for(u32 i = 0; i < count(init_list->list); i++)
    {
        AstNode* node = init_list->list[i];
        auto& member = structure.members[i];

        compile_struct_initializer_item(itl,func,addr_slot,member,node);
    }
}

void compile_struct_designated_initializer_list(Interloper& itl, Function& func, Struct& structure, AddrSlot addr_slot, DesignatedListNode* init_list)
{
    for(auto& initializer : init_list->initializer)
    {
        auto& member = structure.members[initializer.member];
        compile_struct_initializer_item(itl,func,addr_slot,member,initializer.expr);
    }
}

void compile_struct_init(Interloper& itl, Function& func, AstNode* node, StructType* struct_type,  AddrSlot* addr_slot)
{
    auto& structure = struct_from_type(itl.struct_table,struct_type);

    switch(node->type)
    {
        case ast_type::initializer_list:
        {
            compile_struct_initializer_list(itl,func,structure,*addr_slot,(InitializerListNode*)node);
            addr_slot->addr.offset += structure.size;
            break;
        }

        case ast_type::designated_initializer_list:
        {
            compile_struct_designated_initializer_list(itl,func,structure,*addr_slot,(DesignatedListNode*)node);
            addr_slot->addr.offset += structure.size;
            break;
        }

        case ast_type::no_init:
        {
            addr_slot->addr.offset += structure.size;
            break;
        }

        default:
        {
            // If this is just a plain symbol we don't want to move a copy.
            if(is_direct_addr(*addr_slot))
            {
                compile_expression(itl,func,node,addr_slot->addr.base);
                addr_slot->addr.offset += structure.size;
                return;
            }

            auto reg = compile_oper(itl,func,node);

            const TypedAddr dst_addr = {*addr_slot,(Type*)struct_type};
            do_addr_store(itl,func,reg,dst_addr);
            addr_slot->addr.offset += structure.size;
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
    compile_struct_init(itl,func,decl_node->expr,struct_type,&addr_slot);
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
            // Fixed size handled by known value.
            addr->type = itl.usize_type;
            addr->addr_slot.addr.offset += GPR_SIZE;
            break;
        }
    }
}

void access_index_member(Interloper& itl, Function& func,TypedAddr* struct_addr, const AccessMember& access_member)
{
    const auto& structure = struct_from_type(itl.struct_table,(StructType*)struct_addr->type);
    const auto& member = structure.members[access_member.member];

    struct_addr->type = member.type;
    // fixed size should just collpase the offset
    struct_addr->addr_slot.addr.offset += member.offset;

    if(is_runtime_size(struct_addr->type))
    {
        const RegSlot vla_ptr = new_tmp_ptr(func);
        const TypedAddr src_addr = {struct_addr->addr_slot,make_reference(itl,index_arr(struct_addr->type))};
        
        do_addr_load(itl,func,vla_ptr,src_addr);

        struct_addr->addr_slot = make_pointer_addr(vla_ptr,0);
    }

    IndexNode* index = (IndexNode*)access_member.expr;

    switch(index->type)
    {
        case index_type::pointer:
        {
            const auto ptr_slot = collapse_struct_addr_oper(itl,func,struct_addr->addr_slot);
            *struct_addr = compile_pointer_index(itl,func,index,ptr_slot);
            break;
        }

        case index_type::array:
        {
            *struct_addr = compile_array_index(itl,func,index,(ArrayType*)struct_addr->type,struct_addr->addr_slot);
            break;
        }
    }
}

void access_enum_struct_member(Interloper& itl,Function& func, const AccessMember& member_access, TypedAddr* struct_addr)
{
    const auto enumeration = enum_from_type(itl.enum_table, (EnumType*)struct_addr->type);
    const auto& structure = struct_from_type(itl.struct_table,(StructType*)enumeration.underlying_type);
    const auto& member = structure.members[member_access.member];

    // get the start of the table
    const auto enum_table_slot = pool_addr_res(itl,func,enumeration.struct_slot,0);

    // get the enum index
    RegSlot enum_slot = INVALID_SYM_REG_SLOT;
    
    // we allready directly have the enum
    if(struct_addr->addr_slot.struct_addr)
    {
        assert(struct_addr->addr_slot.addr.offset == 0);
        enum_slot = struct_addr->addr_slot.addr.base;
    }

    // ordinary access on a pointer, we must deref it
    else
    {
        enum_slot = new_tmp(func,GPR_SIZE);
        load_addr_slot(itl,func,enum_slot,struct_addr->addr_slot,ENUM_SIZE,false,false);
    }

    // finally index the table
    const auto addr = generate_indexed_pointer(itl,func,enum_table_slot,enum_slot,structure.size,member.offset);
    struct_addr->addr_slot = addr;
    struct_addr->type = member.type;
}

void access_slice_member(Interloper& itl,Function& func, const AccessMember& member_access, TypedAddr* struct_addr)
{
    SliceNode* slice_node = (SliceNode*)member_access.expr;

    const auto& structure = struct_from_type(itl.struct_table,(StructType*)struct_addr->type);
    const auto& member = structure.members[member_access.member];

    struct_addr->addr_slot.addr.offset += member.offset;
    struct_addr->type = member.type;

    const RegSlot dst_slot = new_struct(func,GPR_SIZE * 2);
    compile_array_slice(itl,func,slice_node,*struct_addr,dst_slot);

    *struct_addr = {make_struct_addr(dst_slot,0), member_access.expr_type};
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
                const auto& structure = struct_from_type(itl.struct_table,(StructType*)struct_addr.type);
                const auto& member = structure.members[access_member.member];

                struct_addr.addr_slot.addr.offset += member.offset;  
                struct_addr.type = member.type;
                break;
            }

            case member_access_type::enum_t:
            {
                access_enum_struct_member(itl, func, access_member, &struct_addr);
                break;
            }

            case member_access_type::array_t:
            {
                const auto array_member = array_member_access(access_member.member);
                access_array_member(itl,&struct_addr,array_member);
                break;
            }

            case member_access_type::slice_t:
            {
                access_slice_member(itl, func, access_member, &struct_addr);
                break;
            }

            case member_access_type::index_t:
            {
                access_index_member(itl,func,&struct_addr,access_member);
                break;
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

    if(struct_access->flags & FIXED_ARRAY_ACCESS_DATA_FLAG)
    {
        collapse_struct_addr(itl,func,dst_slot,src_addr.addr_slot);
        return;
    }

    do_addr_load(itl,func,dst_slot,src_addr);
}

void compile_struct_access(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    read_struct(itl,func,(StructAccessNode*)expr,dst_slot);
}

void compile_struct_initializer(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    StructInitializerNode* init = (StructInitializerNode*)expr;

    AddrSlot dst = make_struct_addr(dst_slot,0);
    StructType* type = (StructType*)init->node.expr_type;
    compile_struct_init(itl,func,init->initializer,type,&dst);
}

void compile_struct_return(Interloper& itl, Function& func, AstNode* stmt)
{
    StructInitializerNode* init = (StructInitializerNode*)stmt;

    AddrSlot dst = make_pointer_addr(make_sym_reg_slot(func.sig.args[0]),0);
    StructType* type = (StructType*)init->node.expr_type;
    compile_struct_init(itl,func,init->initializer,type,&dst);

    ret(itl,func);
}

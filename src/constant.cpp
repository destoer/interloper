
b32 is_constant(const Symbol& sym)
{
    return sym.reg.kind == reg_kind::constant;
}

// NOTE: type checking rules are less strict than in "runtime" expressions
void check_const_cmp(Interloper& itl, Type* ltype, Type* rtype, logic_op type)
{
    b32 valid = false;

    // only allowed on bools
    if(type == logic_op::or_reg || type == logic_op::and_reg)
    {
        valid = is_bool(ltype) && is_bool(rtype);
    }

    else
    {
        valid = (is_integer(ltype) && is_integer(rtype)) || (is_bool(ltype) && is_bool(rtype));
    }

    if(!valid)
    {
        panic(itl,itl_error::const_type_error,"Could not compare types: %s : %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }
}

struct ConstData
{
    // actual data repr
    union
    {
        // inline data
        // enum, builtin types
        u64 v = 0;

        // Pointer to other const pool var
        // Used to return arrays, pointers structs
        ConstDataPointer data_pointer;
    };

    // NOTE: this indicates how to access const data, as described above
    Type* type = nullptr;
};

ConstData make_const_builtin(u64 v, Type* type)
{
    ConstData data;

    data.v = v;
    data.type = type;

    return data;
}


void write_const_builtin(Interloper& itl,PoolSlot slot, u32 offset,const ConstData& data)
{
    auto& section = pool_section_from_slot(itl.const_pool,slot);

    // calc the read reqs
    const u32 addr = section.offset + offset;
    const u32 size = type_size(itl,data.type);

    switch(size)
    {
        case 1:
        {
            write_mem<u8>(itl.const_pool.buf,addr,data.v);
            break;
        }

        case 2:
        {
            write_mem<u16>(itl.const_pool.buf,addr,data.v);
            break;
        }

        case 4:
        {
            write_mem<u32>(itl.const_pool.buf,addr,data.v);
            break;
        }

        case 8:
        {
            write_mem<u64>(itl.const_pool.buf,addr,data.v);
            break;
        }

        default:
        {
            crash_and_burn("invalid sized const builtin write");
            break;
        }
    }
}

// used for writing into compound data, i.e structs, arrays
void write_const_data(Interloper& itl, PoolSlot slot, u32 offset, const ConstData& data)
{
    // write out based on type
    Type* type = data.type;

    // builtin type can just be directly read out as is from here
    if(is_builtin(type))
    {
        write_const_builtin(itl,slot,offset,data);
    }

    // this is technically just an int
    // return in place
    else if(is_enum(type))
    {
        assert(false);
    }

    
    else if(is_array(type))
    {
        // this should only be legal for vla assigns

        assert(false);
    }

    else if(is_struct(type))
    {
        assert(false);
    }

    else if(is_pointer(type))
    {
        assert(false);
    }

    // whoops
    else
    {
        assert(false);
    }   
}

PoolSlot pool_slot_from_sym(const Symbol& sym)
{
    return pool_slot_from_idx(sym.reg.offset);
}

u64 builtin_from_const(Interloper& itl, Type* type,PoolSlot slot, u32 offset)
{
    auto& section = pool_section_from_slot(itl.const_pool,slot);

    // calc the read reqs
    const u32 addr = section.offset + offset;
    const u32 size = type_size(itl,type);

    switch(size)
    {
        case 1:
        {
            return read_mem<u8>(itl.const_pool.buf,addr);
        }

        case 2:
        {
            return read_mem<u16>(itl.const_pool.buf,addr);
        }

        case 4:
        {
            return read_mem<u32>(itl.const_pool.buf,addr);
        }

        case 8:
        {
            return read_mem<u64>(itl.const_pool.buf,addr);
        }

        default:
        {
            crash_and_burn("invalid sized const builtin read");
        }
    }
}

ConstData read_const_data(Interloper& itl, Type* type, PoolSlot slot, u32 offset)
{
    // read out based on type

    // builtin type can just be directly read out as is from here
    if(is_builtin(type))
    {
        ConstData data;

        data.v = builtin_from_const(itl,type,slot,offset);
        data.type = type;

        return data;
    }

    // this is technically just an int
    // return in place
    else if(is_enum(type))
    {
        assert(false);
    }

    // return the array pointer
    // NOTE: this works the same as arrays normally
    // if this is a vla we will get a pointer to the struct
    // for fixed arrays this is the data pointer
    else if(is_array(type))
    {
        assert(false);
    }

    // return slot
    else if(is_struct(type))
    {
        assert(false);
    }

    // return pointer data
    else if(is_pointer(type))
    {
        assert(false);
    }

    // whoops
    else
    {
        assert(false);
    }        
}

ConstData read_const_sym(Interloper& itl, Symbol& sym)
{
    const auto pool_slot = pool_slot_from_idx(sym.reg.offset);

    return read_const_data(itl,sym.type,pool_slot,0);
}


ConstData compile_const_expression(Interloper& itl, AstNode* node)
{
    switch(node->type)
    {
        case ast_type::value:
        {
            ValueNode* value_node = (ValueNode*)node;
            Value value = value_node->value;

            return make_const_builtin(value.v,value_type(itl,value));               
        }

        case ast_type::char_t:
        {
            CharNode* char_node = (CharNode*)node;

            return make_const_builtin(char_node->character,make_builtin(itl,builtin_type::c8_t));            
        }

        case ast_type::symbol:
        {

            // TODO: atm this requires correct decl order
            // as we dont have a locking mechanism or a way to lookup exprs
            auto [type,sym_slot] = symbol(itl,node);

            if(itl.error)
            {
                return make_const_builtin(0,make_builtin(itl,builtin_type::void_t));        
            }

            // pull sym
            auto& sym = sym_from_slot(itl.symbol_table,sym_slot);

            if(!is_constant(sym))
            {
                panic(itl,itl_error::const_type_error,"symbol %s is not constant\n",sym.name.buf);
                return make_const_builtin(0,make_builtin(itl,builtin_type::void_t));
            }

            return read_const_sym(itl,sym);
        }

        case ast_type::plus:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);
            
            if(is_pointer(left.type) && is_integer(right.type))
            {
                unimplemented("const pointer add");
            }

            else
            {
                const u64 ans = left.v + right.v;
                Type* type = effective_arith_type(itl,left.type,right.type,op_type::add_reg);

                return make_const_builtin(ans,type);
            }
        }

        case ast_type::bitwise_and:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);

            const u64 ans = left.v & right.v;
            Type* type = effective_arith_type(itl,left.type,right.type,op_type::and_reg);

            return make_const_builtin(ans,type);   
        }


        case ast_type::shift_l:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);

            const u64 ans = left.v << right.v;
            Type* type = effective_arith_type(itl,left.type,right.type,op_type::lsl_reg);

            return make_const_builtin(ans,type);               
        }

        case ast_type::shift_r:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);

            // TODO: handle asr
            const u64 ans = left.v >> right.v;
            Type* type = effective_arith_type(itl,left.type,right.type,op_type::lsr_reg);

            return make_const_builtin(ans,type);          
        }

        case ast_type::times:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);

            const u64 ans = left.v * right.v;
            Type* type = effective_arith_type(itl,left.type,right.type,op_type::mul_reg);

            return make_const_builtin(ans,type);   
        }

        case ast_type::minus:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);
            
            if(is_pointer(left.type) && is_integer(right.type))
            {
                unimplemented("const pointer sub");
            }

            else
            {
                const u64 ans = left.v - right.v;
                Type* type = effective_arith_type(itl,left.type,right.type,op_type::sub_reg);

                return make_const_builtin(ans,type);
            }
        }

        case ast_type::divide:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);

            if(right.v == 0)
            {
                panic(itl,itl_error::int_type_error,"attempted to divide by zero in const expr\n");
                return make_const_builtin(0,make_builtin(itl,builtin_type::void_t)); 
            }

            const u64 ans = left.v / right.v;
            Type* type = effective_arith_type(itl,left.type,right.type,op_type::sub_reg);

            return make_const_builtin(ans,type);
        }

        case ast_type::logical_eq:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto left = compile_const_expression(itl,bin_node->left);
            const auto right = compile_const_expression(itl,bin_node->right);

            check_const_cmp(itl,left.type,right.type,logic_op::cmpeq_reg);

            const b32 ans = left.v == right.v;

            return make_const_builtin(ans,make_builtin(itl,builtin_type::bool_t));
        }

        case ast_type::access_struct:
        {
            // are we accessing type info on a type name?
            BinNode* member_root = (BinNode*)node;
            AstNode* expr_node = member_root->left;

            if(expr_node->type == ast_type::symbol)
            {
                RecordNode* members = (RecordNode*)member_root->right;

                // potential type info access
                if(count(members->nodes) == 1 && members->nodes[0]->type == ast_type::access_member)
                {
                    LiteralNode* sym_node = (LiteralNode*)expr_node;
                    const auto name = sym_node->literal;

                    TypeDecl* type_decl = lookup_type(itl,name);

                    if(type_decl)
                    {
                        LiteralNode* member_node = (LiteralNode*) members->nodes[0];

                        auto [type,ans] = access_type_info(itl,*type_decl,member_node->literal);
                        return make_const_builtin(ans,type);
                    }
                }
            }

            // ordinary struct access

            panic(itl,itl_error::const_type_error,"struct access not supported in constant expr");
            return make_const_builtin(0,make_builtin(itl,builtin_type::void_t)); 
        }

        default:
        {
            panic(itl,itl_error::const_type_error,"unrecognised operation for const expr: %s\n",AST_NAMES[u32(node->type)]);
            return make_const_builtin(0,make_builtin(itl,builtin_type::void_t));
        }
    }    
}

std::pair<u64,Type*> compile_const_int_expression(Interloper& itl, AstNode* node)
{
    const auto data = compile_const_expression(itl,node);

    // not valid if this is not an int
    if(!is_integer(data.type))
    {
        panic(itl,itl_error::int_type_error,"expected integer for const int expr got %s\n",type_name(itl,data.type).buf);
        return std::pair{0,make_builtin(itl,builtin_type::void_t)}; 
    }

    return std::pair{data.v,data.type};
}


bool compile_const_bool_expression(Interloper& itl, AstNode* node)
{
    const auto data = compile_const_expression(itl,node);

    // not valid if this is not an int
    if(!is_bool(data.type))
    {
        panic(itl,itl_error::int_type_error,"expected bool for const bool expr got %s\n",type_name(itl,data.type).buf);
        return false;
    }

    return bool(data.v);
}

void compile_const_arr_list_internal(Interloper& itl,RecordNode* list, ArrayType* type, PoolSlot slot, u32* offset)
{
    if(itl.error)
    {
        return;
    }

    const u32 node_len = count(list->nodes);

    if(is_runtime_size(type))
    {
        panic(itl,itl_error::array_type_error,"cannot assign initalizer to vla\n");
        return;
    }

    // next type is a sub array
    if(is_array(type->contained_type))
    {
        ArrayType* next_arr = (ArrayType*)type->contained_type;

        const u32 count = type->size;

        // type check the actual ammount is right
        // TODO: this should allow not specifing the full ammount but for now just keep it simple
        if(count != node_len)
        {
            panic(itl,itl_error::missing_initializer,"array %s expects %d initializers got %d\n",type_name(itl,(Type*)type).buf,count,node_len);
            return;
        }        

        for(u32 n = 0; n < node_len; n++)
        {
            AstNode* node = list->nodes[n];

            // descend each sub initializer until we hit one containing values
            // for now we are just gonna print them out, and then we will figure out how to emit the inialzation code
            switch(node->type)
            {
                case ast_type::initializer_list:
                {
                    compile_const_arr_list_internal(itl,(RecordNode*)node,next_arr,slot,offset);
                    break;
                }

                case ast_type::string:
                {
                    unimplemented("const array string");
                }

                // handle an array (this should fufill the current "depth req in its entirety")
                default:
                {
                    unimplemented("arr initializer with array");
                    break;            
                }
            }
        }
    }

    // we are getting to the value assigns!
    else
    {  
        Type* base_type = type->contained_type;

        const u32 size = type_size(itl,base_type);

        // seperate loop incase we need to handle initializers
        if(is_struct(base_type))
        {
            unimplemented("const array of structs");
        }

        // normal types
        else
        {
            for(u32 i = 0; i < node_len; i++)
            {
                auto data = compile_const_expression(itl,list->nodes[i]);
                check_assign_init(itl,base_type,data.type);

                write_const_data(itl,slot,*offset,data);
                *offset = *offset + size;
            }
        }           
    } 
}

PoolSlot add_const_vla(Interloper& itl, Symbol& array, PoolSlot data_slot,u64 len)
{
    static_assert(GPR_SIZE == 8);

    // alloc vla struct
    const auto vla_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,GPR_SIZE * 2);
    auto& vla_section = pool_section_from_slot(itl.const_pool,vla_slot);

    // write in the data
    write_const_pool_pointer(itl.const_pool,vla_section,0,data_slot); 
    write_const_pool(itl.const_pool,vla_section,GPR_SIZE,len);

    array.reg.offset = vla_slot.handle;

    return vla_slot;
}

// returns data slot
PoolSlot add_const_fixed_array(Interloper& itl, Symbol& array)
{
    // okay now reinit subsizes now we know the complete type
    init_arr_sub_sizes(itl,(Type*)array.type);
    
    // preallocate the arr data
    const auto [arr_size,arr_count] = calc_arr_allocation(itl,array);
    const u32 data_size = arr_size * arr_count;

    const auto data_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);

    // add array pointer
    const auto pointer_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,GPR_SIZE);
    auto& pointer_section = pool_section_from_slot(itl.const_pool,pointer_slot);

    // mark as location
    array.reg.offset = pointer_slot.handle;

    write_const_pool_pointer(itl.const_pool,pointer_section,0,data_slot);

    return data_slot;    
}

void compile_const_arr_initializer_list(Interloper& itl, Symbol& array, AstNode* node)
{
    if(is_runtime_size(array.type))
    {
        panic(itl,itl_error::array_type_error,"Cannot use initializer list with vla\n");
        return;
    }

    // scan each part of initializer quickly
    // and determine the size, so we can figure out how
    // much we need to alloc into the pool 
    // NOTE: we still check that the sizes are the same later
    RecordNode* record_node = (RecordNode*)node;

    auto record_cur = record_node;
    ArrayType* type_cur = (ArrayType*)array.type;

    b32 done = false;

    while(!done)
    {
        const u32 size = count(record_cur->nodes);

        if(type_cur->size == DEDUCE_SIZE)
        {
            type_cur->size = size;
        }

        // can we descend any more?
        const b32 valid = is_array(type_cur->contained_type) && (size != 0) && (record_cur->nodes[0]->type == ast_type::initializer_list);

        done = !valid;

        if(valid)
        {
            type_cur = (ArrayType*)type_cur->contained_type;
            record_cur = (RecordNode*)record_cur->nodes[0];
        }
    }

    // prealloc data inside the const pool for this
    const auto data_slot = add_const_fixed_array(itl,array);

    // actually insert the data into the const pool for the list
    u32 offset = 0;
    compile_const_arr_list_internal(itl,record_node,(ArrayType*)array.type,data_slot,&offset);
}


void compile_constant_initializer(Interloper& itl, Symbol& sym, AstNode* node)
{
    // switch on top level expression
    // check it is correct for the kind of type we expect from this assignment
    if(is_builtin(sym.type))
    {
        const builtin_type type = builtin_type(sym.type->type_idx);
        
        switch(type)
        {
            // integers
            case builtin_type::u8_t:
            case builtin_type::u16_t:
            case builtin_type::u32_t:
            case builtin_type::u64_t:
            case builtin_type::s8_t:
            case builtin_type::s16_t:
            case builtin_type::s32_t:
            case builtin_type::s64_t:
            case builtin_type::byte_t:
            case builtin_type::c8_t:
            {
                // compile expression
                auto [v, rtype] = compile_const_int_expression(itl,node);

                // now we know the value get a more accurate value up to max precision
                Value value;
                value.v = v;
                value.sign = is_signed(rtype);

                rtype = value_type(itl,value);

                if(itl.error)
                {
                    return;
                }

                // type check the integer
                check_assign_init(itl,sym.type,rtype);
   
                // push to const pool and save handle as offset for later loading...
                const auto slot = push_const_pool(itl.const_pool,pool_type::var,&v,GPR_SIZE);
                sym.reg.offset = slot.handle;

                break;
            }


            case builtin_type::bool_t:
            {
                const auto res = compile_const_bool_expression(itl,node);

                if(itl.error)
                {
                    return;
                }

                const auto slot = push_const_pool(itl.const_pool,pool_type::var,&res,GPR_SIZE);
                sym.reg.offset = slot.handle;

                break;                
            }

            // these should not be possible...
            case builtin_type::null_t:
            {
                panic(itl,itl_error::undefined_type_oper,"null as dst type in constant expression!?\n");
                return;
            }

            case builtin_type::void_t:
            {
                panic(itl,itl_error::undefined_type_oper,"void as dst type in constant expression!?\n");
                return;
            } 
        }
    }

    else if(is_enum(sym.type))
    {
        assert(false);
    }


    else if(is_array(sym.type))
    {
        // initializer
        switch(node->type)
        {
            case ast_type::initializer_list:
            {
                compile_const_arr_initializer_list(itl,sym,node);
                break;
            }

            case ast_type::string:
            {
                if(!is_string(sym.type))
                {
                    panic(itl,itl_error::string_type_error,"expected string got %s\n",type_name(itl,sym.type).buf);
                    return;
                }

                LiteralNode* literal_node = (LiteralNode*)node;
                const String literal = literal_node->literal;

                ArrayType* array_type = (ArrayType*)sym.type;

                if(is_runtime_size(sym.type))
                {
                    unimplemented("runtime size const string lit");
                }

                // fixed size
                else
                {
                    // handle auto sizing
                    if(array_type->size == DEDUCE_SIZE)
                    {
                        array_type->size = literal.size;
                    }

                    const auto data_slot = add_const_fixed_array(itl,sym); 
                    auto& section = pool_section_from_slot(itl.const_pool,data_slot);

                    // copy the string in
                    write_const_pool(itl.const_pool,section,0,literal.buf,literal.size);           
                }
                break;
            }

            default:
            {
                unimplemented("arbitary assign const array");
                break;
            }
        }
    }

    else if(is_struct(sym.type))
    {
        assert(false);
    }

    else if(is_pointer(sym.type))
    {
        assert(false);
    }

    // whoops
    else
    {
        assert(false);
    }

    // array intializer

    // struct initializer

}


void compile_constant_decl(Interloper& itl, DeclNode* decl_node, b32 global)
{
    // pull type and name so we can create a symbol
    const auto name = decl_node->name;

    if(symbol_exists(itl.symbol_table,name))
    {
        panic(itl,itl_error::redeclaration,"constant symbol %s redefined\n",name.buf);
        return;
    }

    // force constant
    decl_node->type->is_constant = true;

    // build the typing info
    Type* type = get_type(itl,decl_node->type);

    // add into table
    auto& sym = global? add_global(itl,name,type,true) : add_symbol(itl,name,type);

    // compile the expression
    compile_constant_initializer(itl,sym,decl_node->expr);    
}

void compile_constant(Interloper& itl, GlobalDeclNode* node)
{
    // setup the correct file for error reporting
    itl.cur_file = node->filename;

    itl.cur_expr = (AstNode*)node;

    compile_constant_decl(itl,node->decl,true);
}

void compile_constants(Interloper& itl)
{
    for(u32 c = 0; c < count(itl.constant_decl); c++)
    {
        compile_constant(itl,itl.constant_decl[c]);

        if(itl.error)
        {
            return;
        }
    }
}
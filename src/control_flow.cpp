

dtr_res compile_if_block(Interloper &itl,Function &func,AstNode *node)
{
    BlockNode* if_block = (BlockNode*)node;

    BlockSlot start_block = cur_block(func);
    BlockSlot exit_block;

    for(u32 n = 0; n < count(if_block->statements); n++)
    {
        BinNode* if_stmt = (BinNode*)if_block->statements[n];

        // compile the compare expr if conditon
        auto res = compile_oper(itl,func,if_stmt->left);
        if(!res)
        {
            return dtr_res::err;
        }

        auto [type,reg] = *res;

        // integer or pointer is fine check they aernt zero as a shorthand
        if(is_integer(type) || is_pointer(type))
        {
            reg = cmp_ne_imm_res(itl,func,reg,0);
        }
        
        else if(!is_bool(type))
        {
            compile_error(itl,itl_error::bool_type_error,"expected bool got %s in if condition\n",type_name(itl,type).buf);
            return dtr_res::err;
        }

        // block for comparison branch
        // cant emit yet as we dont know how may blocks the if statement we are jumping over is yet
        const BlockSlot cmp_block = cur_block(func);

        // compile the body block
        const auto body_slot_opt = compile_basic_block(itl,func,(BlockNode*)if_stmt->right);
        if(!body_slot_opt)
        {
            return dtr_res::err;
        }

        const BlockSlot body_slot = *body_slot_opt;

        // not the last statment 
        if(n != count(if_block->statements) - 1)
        {
            // indicate we need to jump the exit block
            emit_exit_block(itl,func);

            if(if_block->statements[n+1]->type == ast_type::else_t)
            {
                // else stmt has no expr so its in the first node
                // and by definition this is the last statement with no cond so we have to explictly splice a jump to it
                UnaryNode* else_stmt = (UnaryNode*)if_block->statements[n+1];
                const auto else_slot_opt = compile_basic_block(itl,func,(BlockNode*)else_stmt->next);
                if(!else_slot_opt)
                {
                    return dtr_res::err;
                }

                // add branch over body we compiled to else statement
                emit_cond_branch(itl,func,cmp_block,*else_slot_opt,body_slot,reg,false);

                exit_block = add_fall(itl,func);
                break;
            }

            else
            {
                // create new block for compare for the next node
                // TODO: should this have hte type of the initial node or no?
                const BlockSlot chain_slot = new_basic_block(itl,func);

                // add branch over the body we compiled earlier
                emit_cond_branch(itl,func,cmp_block,chain_slot,body_slot,reg,false);
            }
        }

        // Final block, this exit is done via a fallthrough
        else
        {
            exit_block = add_fall(itl,func);

            // if cond not met just branch into exit block
            emit_cond_branch(itl,func,cmp_block,exit_block,body_slot,reg,false);          
        }
    }

    // now we have to exit block add in every exit
    auto &blocks = func.emitter.program;

    for(u32 b = start_block.handle; b < count(blocks) - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.list.finish)
        {
            if(block.list.finish->value.op == op_type::exit_block)
            {
                remove(block.list,block.list.finish);
                emit_branch(itl,func,block_from_idx(b),exit_block);
            }
        }
    }

    return dtr_res::ok;
}

dtr_res compile_while_block(Interloper &itl,Function &func,AstNode *node)
{
    BinNode* while_node = (BinNode*)node;

    // compile cond
    auto entry_res = compile_oper(itl,func,while_node->left);
    if(!entry_res)
    {
        return dtr_res::err;
    }

    auto [cond_type,entry_cond] = *entry_res;

    const BlockSlot initial_block = cur_block(func);

    // integer or pointer, check not zero
    if(is_integer(cond_type) || is_pointer(cond_type))
    {
        entry_cond = cmp_ne_imm_res(itl,func,entry_cond,0);
    }

    // check cond is actually a bool
    else if(!is_bool(cond_type))
    {
        compile_error(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,cond_type).buf);
        return dtr_res::err;
    }    

    // compile body
    const auto while_block_opt = compile_basic_block(itl,func,(BlockNode*)while_node->right); 
    if(!while_block_opt)
    {
        return dtr_res::err;
    }

    const BlockSlot while_block = *while_block_opt;

    RegSlot exit_cond;
    auto exit_res = compile_oper(itl,func,while_node->left);
    if(!exit_res)
    {
        return dtr_res::err;
    }

    std::tie(std::ignore,exit_cond) = exit_res.value();

    // integer or pointer, check not zero
    if(is_integer(cond_type) || is_pointer(cond_type))
    {
        exit_cond = cmp_ne_imm_res(itl,func,exit_cond,0);
    }

    const BlockSlot end_block = cur_block(func);

    const BlockSlot exit_block = new_basic_block(itl,func);

    // keep looping to while block if cond is true
    emit_cond_branch(itl,func,end_block,while_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,while_block,entry_cond,false); 
    return dtr_res::ok;  
}

dtr_res compile_for_range_idx(Interloper& itl, Function& func, ForRangeNode* for_node, b32 inc, op_type cmp_type)
{
    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    BinNode* cmp_node = (BinNode*)for_node->cond;

    // grab end
    // NOTE: we need to regrab this later incase it is not a const
    const auto entry_res = compile_oper(itl,func,cmp_node->right);
    if(!entry_res)
    {
        return dtr_res::err;
    }

    const auto [entry_end_type,entry_end] = *entry_res;

    // make index the same sign as the end stmt
    const auto& sym = add_symbol(itl,for_node->name_one,make_builtin(itl,inc? builtin_type::u32_t : builtin_type::s32_t)); 

    const RegSlot index = sym.reg.slot;

    // grab initalizer
    const auto entry_init_type_opt = compile_expression(itl,func,cmp_node->left,index);
    if(!entry_init_type_opt)
    {
        return dtr_res::err;
    }

    const Type* entry_init_type = *entry_init_type_opt;

    if(!is_integer(entry_init_type) || !is_integer(entry_end_type))
    {
        compile_error(itl,itl_error::bool_type_error,"expected integer's in range conditon got %s,%s\n",
            type_name(itl,entry_init_type).buf,type_name(itl,entry_init_type).buf);
        return dtr_res::err;
    }    

    // compile in cmp for entry
    RegSlot entry_cond = new_tmp(func,GPR_SIZE);
    emit_block_internal_slot(func,initial_block,cmp_type,entry_cond,index,entry_end);

    // compile the main loop body
    const auto for_block_opt = compile_basic_block(itl,func,for_node->block); 
    if(!for_block_opt)
    {
        return dtr_res::err;
    }

    const BlockSlot for_block = *for_block_opt;

    // compile post inc / dec
    if(inc)
    {
        add_imm(itl,func,index,index,1);
    }

    else
    {
        sub_imm(itl,func,index,index,1);
    }

    // compile body check
    const BlockSlot end_block = cur_block(func);

    // regrab end
    const auto exit_res = compile_oper(itl,func,cmp_node->right);
    if(!exit_res)
    {
        return dtr_res::err;
    }

    const auto [exit_end_type,exit_end] = *exit_res;

    RegSlot exit_cond = new_tmp(func,GPR_SIZE);
    emit_block_internal_slot(func,end_block,cmp_type,exit_cond,index,exit_end);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry_cond,false);

    return dtr_res::ok;    
}

dtr_res compile_for_range_arr(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    const auto entry_res = compile_expression_tmp(itl,func,for_node->cond);
    if(!entry_res)
    {
        return dtr_res::err;
    }

    const auto [type, arr_slot] = *entry_res;

    if(!is_array(type))
    {
        compile_error(itl,itl_error::array_type_error,"Expected array for range stmt got %s\n",type_name(itl,type).buf);
        return dtr_res::err;
    }

    ArrayType* arr_type = (ArrayType*)type;

    const u32 index_size = arr_type->sub_size;

    // attempting to index statically zero array
    // we dont care
    if(is_fixed_array(arr_type) && arr_type->size == 0)
    {
        return dtr_res::ok;
    }

    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    const b32 track_idx = for_node->name_two.buf != nullptr;
    RegSlot index = make_spec_reg_slot(spec_reg::null);
    RegSlot data = make_spec_reg_slot(spec_reg::null);

    // create var to hold data
    if(!for_node->take_pointer)
    {
        const auto& sym = add_symbol(itl,for_node->name_one,index_arr(arr_type));
        data = sym.reg.slot;
    }

    else
    {
        const auto& sym = add_symbol(itl,for_node->name_one,make_reference(itl,index_arr(arr_type)));
        data = sym.reg.slot;
    }
    

    if(track_idx)
    {
        // check our loop index does not exist
        if(symbol_exists(itl.symbol_table,for_node->name_two))
        {
            compile_error(itl,itl_error::redeclaration,"redeclared index in for range loop: %s\n",for_node->name_one.buf);
            return dtr_res::err;
        }        

        // make a fake constant symbol
        auto& sym = add_symbol(itl,for_node->name_two,make_builtin(itl,builtin_type::u32_t,true));
        sym.reg.flags &= ~CONST;

        // init the index
        index = sym.reg.slot;
        mov_imm(itl,func,index,0);
    }

    // setup the loop grab data and len
    const auto arr_len = load_arr_len(itl,func,arr_slot,type);
    const auto arr_bytes = mul_imm_res(itl,func,arr_len,index_size);

    const auto arr_data = load_arr_data(itl,func,arr_slot,type);

    // compute array end
    const auto arr_end = add_res(itl,func,arr_data,arr_bytes);

    RegSlot entry_cond = make_spec_reg_slot(spec_reg::null);

    // if this is a fixed size array we dont need to check it
    // on entry apart from the zero check handled above ^
    // because we know it has members
    if(is_runtime_size(arr_type))
    {
        // check array is not empty
        entry_cond = cmp_ne_res(itl,func,arr_data,arr_end);
    }
    
    // compile the main loop body
    const BlockSlot for_block = new_basic_block(itl,func);

    // if we want the data actually load it for us
    if(!for_node->take_pointer)
    {
        // insert the array load inside the for block
        // before we compile the actual stmts
        if(!do_ptr_load(itl,func,data,arr_data,arr_type->contained_type))
        {
            return dtr_res::err;
        }
    }

    // move the pointer in the data
    else
    {
        mov_reg(itl,func,data,arr_data);
    }

    if(!compile_block(itl,func,for_node->block))
    {
        return dtr_res::err;
    }

    // compile body check
    const BlockSlot end_block = cur_block(func);

    // goto next array index
    add_imm(itl,func,arr_data,arr_data,index_size);

    if(track_idx)
    {
        add_imm(itl,func,index,index,1);
    }

    const auto exit_cond = cmp_ne_res(itl,func,arr_data,arr_end);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit_cond,true);

    if(is_runtime_size(arr_type))
    {
        // emit branch over the loop body if array is empty
        emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry_cond,false);  
    }

    // fixed size array only need to add a fall
    // as we know its not empty by this point
    else
    {
        add_block_exit(func,initial_block,for_block);
    }

    return dtr_res::ok;
}


dtr_res compile_for_range(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    // scope for any var decls in the stmt
    enter_new_anon_scope(itl.symbol_table);

    // check our loop index does not exist
    if(symbol_exists(itl.symbol_table,for_node->name_one))
    {
        compile_error(itl,itl_error::redeclaration,"redeclared index in for range loop: %s\n",for_node->name_one.buf);
        return dtr_res::err;
    }

    // determine what kind of loop term we have
    switch(for_node->cond->type)
    {
        // <= or < is inc
        case ast_type::logical_lt:
        {
            return compile_for_range_idx(itl,func,for_node,true,op_type::cmpslt_reg);
        }

        case ast_type::logical_le:
        {
            return compile_for_range_idx(itl,func,for_node,true,op_type::cmpsle_reg);
        }

        // >= or > is dec
        case ast_type::logical_gt:
        {
            return compile_for_range_idx(itl,func,for_node,false,op_type::cmpsgt_reg);
        }

        case ast_type::logical_ge:
        {
            return compile_for_range_idx(itl,func,for_node,false,op_type::cmpsge_reg);
        }

        // array index!
        default:
        {
            return compile_for_range_arr(itl,func,for_node);
        }
    }

    destroy_scope(itl.symbol_table);
}

dtr_res compile_for_iter(Interloper& itl, Function& func, ForIterNode* for_node)
{
    // scope for any var decls in the stmt
    enter_new_anon_scope(itl.symbol_table);

   // compile the first stmt (ussualy an assign)

    
    const auto type = for_node->initializer->type;

    // handle this being a declaration
    switch(type)
    {
        case ast_type::auto_decl:
        {
            if(!compile_auto_decl(itl,func,for_node->initializer))
            {
                return dtr_res::err;
            }
            break;
        }

        case ast_type::declaration:
        {
            if(!compile_decl(itl,func,for_node->initializer))
            {
                return dtr_res::err;
            }
            break;
        }

        default:
        {
            compile_expression_tmp(itl,func,for_node->initializer);
            break;
        }
    }

    // compile cond for entry and check it is a bool
    const auto entry_res = compile_oper(itl,func,for_node->cond);
    if(!entry_res)
    {
        return dtr_res::err;
    }

    const auto [cond_type,entry_cond] = *entry_res;

    const BlockSlot initial_block = cur_block(func);
  
    if(!is_bool(cond_type))
    {
        compile_error(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,cond_type).buf);
        return dtr_res::err;
    }    


    // compile the body
    const auto for_block_opt = compile_basic_block(itl,func,for_node->block);    
    if(!for_block_opt)
    {
        return dtr_res::err;
    }

    const BlockSlot for_block = *for_block_opt;

    // compile loop end stmt
    compile_expression_tmp(itl,func,for_node->post);
    
    RegSlot exit_cond;
    const auto exit_res = compile_oper(itl,func,for_node->cond);
    if(!exit_res)
    {
        return dtr_res::err;
    }

    std::tie(std::ignore,exit_cond) = exit_res.value();

    const BlockSlot end_block = cur_block(func);


    const BlockSlot exit_block = new_basic_block(itl,func);

    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry_cond,false);

    destroy_scope(itl.symbol_table);

    return dtr_res::ok;    
}


dtr_res compile_switch_block(Interloper& itl,Function& func, AstNode* node)
{
    SwitchNode* switch_node = (SwitchNode*)node;


    const u32 size = count(switch_node->statements);


    enum class switch_kind
    {
        integer,
        enum_t,
    };

    switch_kind switch_type;

    Enum* enumeration = nullptr;

    if(size == 0)
    {
        compile_error(itl,itl_error::missing_args,"Switch statement has no cases");
        return dtr_res::err;
    }

    CaseNode* first_case = (CaseNode*)switch_node->statements[0];
    
    // collapse all the values of the statements now we have parsed everything
    // NOTE: we cant do this as the ast is built, as we want to allow usage of values that are defined "out of order"
    switch(first_case->statement->type)
    {
        // enum
        case ast_type::scope:
        {
            ScopeNode* first_stmt = (ScopeNode*)first_case->statement;

            EnumMember* enum_member = nullptr;
            // TODO: Technically we should check for namespaced values
            auto decode_res = decode_enum(itl,first_stmt,&enumeration,&enum_member);

            if(decode_res != enum_decode_res::ok)
            {
                itl.ctx.expr = (AstNode*)first_stmt;
                compile_error(itl,itl_error::enum_type_error,"invalid enum expression: %s\n",enum_decode_msg(decode_res));
                return dtr_res::err;
            }

            for(u32 i = 0; i < size; i++)
            {
                CaseNode* case_node = switch_node->statements[i];

                // check we have a matching enum
                if(case_node->statement->type != ast_type::scope)
                {
                    itl.ctx.expr = (AstNode*)case_node;
                    compile_error(itl,itl_error::enum_type_error,"switch: one or more cases are not an enum\n");
                    return dtr_res::err;
                }

                ScopeNode* scope_node = (ScopeNode*)case_node->statement;

                Enum* case_enum = nullptr;
                decode_res = decode_enum(itl,scope_node,&case_enum,&enum_member);

                if(decode_res != enum_decode_res::ok)
                {
                    compile_error(itl,itl_error::enum_type_error,"invalid enum expression: %s\n",enum_decode_msg(decode_res));
                    return dtr_res::err;
                }


                if(case_enum->type_idx != enumeration->type_idx)
                {
                    compile_error(itl,itl_error::enum_type_error,"differing enums %s : %s in switch statement\n",
                        enumeration->name.buf,case_enum->name.buf);
                    return dtr_res::err;
                }

                case_node->value = enum_member->value;          
            }            

            switch_type = switch_kind::enum_t;
            break;
        }

        // integer expression
        default:
        {
            for(u32 i = 0; i < size; i++)
            {
                CaseNode* case_node = switch_node->statements[i];

                const auto res = compile_const_int_expression(itl,case_node->statement);
                if(!res)
                {
                    return dtr_res::err;
                }

                const auto [case_value,case_type] = *res;

                case_node->value = case_value;             
            }

            switch_type = switch_kind::integer;
            break;            
        }
    }


    // sort the statements, so we can pull out the gaps
    // and binary search them if we need to when emiting the statement dispatcher
    heap_sort(switch_node->statements,[](const CaseNode* v1, const CaseNode* v2)
    {
        return v1->value > v2->value;
    });

    // check every statement on a enum has a handler
    if(switch_type == switch_kind::enum_t && !switch_node->default_statement && size != enumeration->member_map.size)
    {
        // TODO: print the missing cases
        compile_error(itl,itl_error::undeclared,"switch on enum %s missing cases:\n",enumeration->name.buf);
        return dtr_res::err;     
    }


    u64 gap = 0;

    // gap check all the statements and figure out 
    // if they are close enough to encode as a binary table
    // or if a binary search should be employed instead
    for(u32 i = 0; i < size - 1; i++)
    {
        const u64 cur_gap = switch_node->statements[i + 1]->value - switch_node->statements[i]->value;

        // these statements have no gap, this means they are duplicated
        if(cur_gap == 0)
        {
            compile_error(itl,itl_error::redeclaration,"duplicate case %d\n",switch_node->statements[i]->value);
            return dtr_res::err;
        }

        gap += cur_gap;
    }


    // get the number of extra gaps
    gap -= size - 1;

    // TODO: measure what a good value for this is
    static constexpr u32 JUMP_TABLE_LIMIT = 64;


    // TODO: support doing a hybrid approach, of dividing into binary tree searching
    // on jump tables


    // use a jump table
    if(gap < JUMP_TABLE_LIMIT)
    {
        // get the table limits i.e min max
        const s64 min = switch_node->statements[0]->value;
        const s64 max = switch_node->statements[size - 1]->value;
        const u64 range = (max - min) + 1;


        // compile the actual switch expr
        const auto switch_res = compile_oper(itl,func,switch_node->expr);
        if(!switch_res)
        {
            return dtr_res::err;
        }

        const auto [rtype,expr_slot] = *switch_res;

        // type check the switch stmt
        switch(switch_type)
        {
            case switch_kind::integer:
            {
                if(!is_integer(rtype))
                {
                    compile_error(itl,itl_error::int_type_error,"expected integer for switch statement got %s\n",type_name(itl,rtype).buf);
                    return dtr_res::err;
                }
                break;
            }

            case switch_kind::enum_t:
            {
                if(is_enum(rtype))
                {
                    EnumType* enum_type = (EnumType*)rtype;
                    if(enum_type->enum_idx != enumeration->type_idx)
                    {
                        compile_error(itl,itl_error::enum_type_error,"expected enum of type %s got %s\n",enumeration->name.buf,type_name(itl,rtype));
                        return dtr_res::err;                        
                    }
                }

                else
                {
                    compile_error(itl,itl_error::enum_type_error,"expected enum of type %s got %s\n",enumeration->name.buf,type_name(itl,rtype));
                    return dtr_res::err;                    
                }
                break;
            }
        }

        // save our cur block so we can emit the default block dispatch later
        const BlockSlot range_block = cur_block(func);

        // finally emit the dispatch on the table now we know where to exit if the table bounds get execeeded
        const RegSlot switch_slot = new_tmp(func,GPR_SIZE);
        sub_imm(itl,func,switch_slot,expr_slot,min);

        const RegSlot default_cmp = new_tmp(func,GPR_SIZE);
        cmp_unsigned_gt_imm(itl,func,default_cmp,switch_slot,max - min);

        // NOTE: branch is emitted later as we dont know where it goes yet

        // emit the switch table dispatch
        const BlockSlot dispatch_block = new_basic_block(itl,func);

        // mulitply to get a jump table index
        const RegSlot table_index = mul_imm_res(itl,func,switch_slot,GPR_SIZE);

        
        // reserve space for the table inside the constant pool
        const PoolSlot pool_slot = reserve_const_pool_section(itl.const_pool,pool_type::jump_table,GPR_SIZE * range);
        const RegSlot table_addr = pool_addr_res(itl,func,pool_slot,0);

        // get address in the tabel we want
        const RegSlot final_offset = add_res(itl,func,table_addr,table_index);

        // load the address out of the jump table
        const RegSlot target = new_tmp_ptr(func);
        load_ptr(itl,func,target, final_offset,0, GPR_SIZE,false,false);

        // branch on it
        branch_reg(itl,func,target);

        // finally compile all the blocks, and populate the jump table

        // compile each stmt block
        for(u32 i = 0; i < size; i++)
        {
            CaseNode* case_node = switch_node->statements[i];

            const auto case_slot_opt = compile_basic_block(itl,func,case_node->block);
            if(!case_slot_opt)
            {
                return dtr_res::err;
            }

            const BlockSlot case_slot = *case_slot_opt;

            // add link from dispatch to case
            add_block_exit(func,dispatch_block,case_slot);

            const Block& case_block = block_from_slot(func,case_slot);
            case_node->label = case_block.label_slot;

            case_node->end_block = cur_block(func);
        }




        // NOTE: as default is allways the last block it does not need to have a jump to the exit
        BlockSlot default_block;


        // if there is no default then our exit label is the end
        if(switch_node->default_statement)
        {
            const auto default_block_opt = compile_basic_block(itl,func,(BlockNode*)switch_node->default_statement->next);
            if(!default_block_opt)
            {
                return dtr_res::err;
            }

            default_block = *default_block_opt;

            add_block_exit(func,dispatch_block,default_block);
        }

        BlockSlot default_block_end = cur_block(func);

        // create a exit block for every case to jump to when its done
        const BlockSlot exit_block = new_basic_block(itl,func);

        // if there is no explicit default the default is just after the switch ends
        if(!switch_node->default_statement)
        {
            default_block = exit_block;
        }

        else
        {
            // default falls through to exit
            add_block_exit(func,default_block_end,exit_block);
        }


        // we have default posisiton now we can emit the branch for the range checking failing
        emit_cond_branch(itl,func,range_block,default_block,dispatch_block,default_cmp,true);



        // populate the jump table
        u32 case_idx = 0;

        
        const LabelSlot default_label = block_from_slot(func,default_block).label_slot; 

        // NOTE: we do a second pass because we did not know where the default block is stored when
        // we compiled all the switch statements
        for(u32 i = 0; i < range; i++)
        {
            const u32 addr = i * GPR_SIZE;

            // this is a non default case
            CaseNode* case_node = switch_node->statements[case_idx];

            // current jump table entry matches case
            if(case_node->value - min == i)
            {
                //printf("case %ld -> %d L%d\n",case_node->value,addr,case_node->label.handle);

                write_const_pool_label(itl.const_pool,pool_slot, addr, case_node->label);
                case_idx++;

                // add jump to the exit block
                emit_branch(itl,func,case_node->end_block,exit_block);
            }

            // as statements as sorted this means there is no match emit default
            else
            {
                //printf("case %d -> %d default(L%d)\n",i,addr,default_label.handle);

                write_const_pool_label(itl.const_pool,pool_slot, addr, default_label);
            }
        }
    }


    // use a binary search
    else
    {
        unimplemented("binary search");
    }

    return dtr_res::ok;
}
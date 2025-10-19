

Option<itl_error> compile_if_block(Interloper &itl,Function &func,AstNode *node)
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
            return res.error();
        }

        auto cond = *res;

        // integer or pointer is fine check they aernt zero as a shorthand
        if(is_integer(cond.type) || is_pointer(cond.type))
        {
            cond.slot = cmp_ne_imm_res(itl,func,cond.slot,0);
        }

        // Non empty array
        else if(is_array(cond.type))
        {
            const auto len = load_arr_len(itl,func,cond);
            cond.slot = cmp_ne_imm_res(itl,func,len,0);
        }

        else if(!is_bool(cond.type))
        {
            return compile_error(itl,itl_error::bool_type_error,"expected bool got %s in if condition",type_name(itl,cond.type).buf);
        }

        // block for comparison branch
        // cant emit yet as we dont know how may blocks the if statement we are jumping over is yet
        const BlockSlot cmp_block = cur_block(func);

        // compile the body block
        const auto body_slot_res = compile_basic_block(itl,func,(BlockNode*)if_stmt->right);
        if(!body_slot_res)
        {
            return body_slot_res.error();
        }

        const BlockSlot body_slot = *body_slot_res;

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
                const auto else_slot_res = compile_basic_block(itl,func,(BlockNode*)else_stmt->next);
                if(!else_slot_res)
                {
                    return else_slot_res.error();
                }

                // add branch over body we compiled to else statement
                emit_cond_branch(itl,func,cmp_block,*else_slot_res,body_slot,cond.slot,false);

                exit_block = add_fall(itl,func);
                break;
            }

            else
            {
                // create new block for compare for the next node
                // TODO: should this have hte type of the initial node or no?
                const BlockSlot chain_slot = new_basic_block(itl,func);

                // add branch over the body we compiled earlier
                emit_cond_branch(itl,func,cmp_block,chain_slot,body_slot,cond.slot,false);
            }
        }

        // Final block, this exit is done via a fallthrough
        else
        {
            exit_block = add_fall(itl,func);

            // if cond not met just branch into exit block
            emit_cond_branch(itl,func,cmp_block,exit_block,body_slot,cond.slot,false);          
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

    return option::none;
}

Option<itl_error> compile_while_block(Interloper &itl,Function &func,AstNode *node)
{
    BinNode* while_node = (BinNode*)node;

    // compile cond
    auto entry_res = compile_oper(itl,func,while_node->left);
    if(!entry_res)
    {
        return entry_res.error();
    }

    auto entry_cond = *entry_res;

    const BlockSlot initial_block = cur_block(func);

    // integer or pointer, check not zero
    if(is_integer(entry_cond.type) || is_pointer(entry_cond.type))
    {
        entry_cond.slot = cmp_ne_imm_res(itl,func,entry_cond.slot,0);
    }

    // check cond is actually a bool
    else if(!is_bool(entry_cond.type))
    {
        return compile_error(itl,itl_error::bool_type_error,"expected bool got %s in for condition",type_name(itl,entry_cond.type).buf);
    }    

    // compile body
    const auto while_block_res = compile_basic_block(itl,func,(BlockNode*)while_node->right); 
    if(!while_block_res)
    {
        return while_block_res.error();
    }

    const BlockSlot while_block = *while_block_res;

    auto exit_res = compile_oper(itl,func,while_node->left);
    if(!exit_res)
    {
        return exit_res.error();
    }

    auto exit_cond = exit_res.value();

    // integer or pointer, check not zero
    if(is_integer(exit_cond.type) || is_pointer(exit_cond.type))
    {
        exit_cond.slot = cmp_ne_imm_res(itl,func,exit_cond.slot,0);
    }

    const BlockSlot end_block = cur_block(func);

    const BlockSlot exit_block = new_basic_block(itl,func);

    // keep looping to while block if cond is true
    emit_cond_branch(itl,func,end_block,while_block,exit_block,exit_cond.slot,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,while_block,entry_cond.slot,false); 
    return option::none;  
}

Option<itl_error> compile_for_range_idx(Interloper& itl, Function& func, ForRangeNode* for_node, b32 inc, op_type cmp_type)
{
    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    BinNode* cmp_node = (BinNode*)for_node->cond;

    // grab end
    // NOTE: we need to regrab this later incase it is not a const
    const auto entry_res = compile_oper(itl,func,cmp_node->right);
    if(!entry_res)
    {
        return entry_res.error();
    }

    const auto entry_end = *entry_res;

    // make index the same sign as the end stmt
    const auto& sym = add_symbol(itl,for_node->name_one,make_builtin(itl,inc? builtin_type::u32_t : builtin_type::s32_t)); 

    const RegSlot index = sym.reg.slot;

    // grab initializer
    const auto entry_init_type_res = compile_expression(itl,func,cmp_node->left,index);
    if(!entry_init_type_res)
    {
        return entry_init_type_res.error();
    }

    const Type* entry_init_type = *entry_init_type_res;

    if(!is_integer(entry_init_type) || !is_integer(entry_end.type))
    {
        return compile_error(itl,itl_error::bool_type_error,"expected integer's in range condition got %s,%s",
            type_name(itl,entry_init_type).buf,type_name(itl,entry_init_type).buf);
    }    

    // compile in cmp for entry
    RegSlot entry_cond = new_tmp(func,GPR_SIZE);
    const Opcode entry_opcode = make_reg3_instr(cmp_type,entry_cond,index,entry_end.slot);
    emit_block_internal(func,initial_block,entry_opcode);

    // compile the main loop body
    const auto for_block_res = compile_basic_block(itl,func,for_node->block); 
    if(!for_block_res)
    {
        return for_block_res.error();
    }

    const BlockSlot for_block = *for_block_res;

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
        return exit_res.error();
    }

    const auto exit = *exit_res;

    RegSlot exit_cond = new_tmp(func,GPR_SIZE);
    const Opcode exit_opcode = make_reg3_instr(cmp_type,exit_cond,index,exit.slot);
    emit_block_internal(func,end_block,exit_opcode);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry_cond,false);

    return option::none;    
}

Option<itl_error> compile_for_range_arr(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    const auto entry_res = compile_expression_tmp(itl,func,for_node->cond);
    if(!entry_res)
    {
        return entry_res.error();
    }

    const auto entry_arr = *entry_res;

    if(!is_array(entry_arr.type))
    {
        return compile_error(itl,itl_error::array_type_error,"Expected array for range stmt got %s",type_name(itl,entry_arr.type).buf);
    }

    ArrayType* arr_type = (ArrayType*)entry_arr.type;

    const u32 index_size = arr_type->sub_size;

    // attempting to index statically zero array
    // we dont care
    if(is_fixed_array(arr_type) && arr_type->size == 0)
    {
        return option::none;
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
            return compile_error(itl,itl_error::redeclaration,"redeclared index in for range loop: %s",for_node->name_one.buf);
        }        

        // make a fake constant symbol
        auto& sym = add_symbol(itl,for_node->name_two,make_builtin(itl,builtin_type::u32_t,true));
        sym.reg.flags &= ~CONST;

        // init the index
        index = sym.reg.slot;
        mov_imm(itl,func,index,0);
    }

    RegSlot arr_end = {INVALID_HANDLE};

    const auto arr_data = load_arr_data(itl,func,entry_arr);


    if(is_fixed_array(arr_type))
    {
        arr_end = add_imm_res(itl,func,arr_data,arr_type->sub_size * arr_type->size);
    }

    else
    {
        // setup the loop grab data and len
        const auto arr_len = load_arr_len(itl,func,entry_arr);

        const AddrSlot addr = generate_indexed_pointer(itl,func,arr_data,arr_len,index_size,0);
        arr_end = collapse_struct_addr_res(itl,func,addr);
    }


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
        const TypedReg load_reg = {arr_data,arr_type->contained_type};
        const auto load_err = do_ptr_load(itl,func,data,load_reg);
        if(load_err)
        {
            return *load_err;
        }
    }

    // move the pointer in the data
    else
    {
        mov_reg(itl,func,data,arr_data);
    }

    const auto block_err = compile_block(itl,func,for_node->block);
    if(block_err)
    {
        return *block_err;
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

    return option::none;
}


Option<itl_error> compile_for_range(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    // scope for any var decls in the stmt
    auto sym_scope_guard = enter_new_anon_scope(itl.symbol_table);

    // check our loop index does not exist
    if(symbol_exists(itl.symbol_table,for_node->name_one))
    {
        return compile_error(itl,itl_error::redeclaration,"redeclared index in for range loop: %s",for_node->name_one.buf);
    }

    // determine what kind of loop term we have
    switch(for_node->cond->type)
    {
        // array index!
        default:
        {
            assert(false);
        }
    }
}

Option<itl_error> compile_for_iter(Interloper& itl, Function& func, ForIterNode* for_node)
{
    // scope for any var decls in the stmt
    auto sym_scope_guard = enter_new_anon_scope(itl.symbol_table);

   // compile the first stmt (ussualy an assign)

    
    const auto type = for_node->initializer->type;

    // handle this being a declaration
    switch(type)
    {
        default:
        {
            assert(false);
        }
    }

    // compile cond for entry and check it is a bool
    const auto entry_res = compile_oper(itl,func,for_node->cond);
    if(!entry_res)
    {
        return entry_res.error();
    }

    const auto entry = *entry_res;

    const BlockSlot initial_block = cur_block(func);
  
    if(!is_bool(entry.type))
    {
        return compile_error(itl,itl_error::bool_type_error,"expected bool got %s in for condition",type_name(itl,entry.type).buf);
    }    


    // compile the body
    const auto for_block_res = compile_basic_block(itl,func,for_node->block);    
    if(!for_block_res)
    {
        return for_block_res.error();
    }

    const BlockSlot for_block = *for_block_res;

    // compile loop end stmt
    const auto for_post_res = compile_expression_tmp(itl,func,for_node->post);
    if(!for_post_res)
    {
        return for_post_res.error();
    }
    
    const auto exit_res = compile_oper(itl,func,for_node->cond);
    if(!exit_res)
    {
        return exit_res.error();
    }

    auto exit = exit_res.value();

    const BlockSlot end_block = cur_block(func);


    const BlockSlot exit_block = new_basic_block(itl,func);

    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit.slot,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry.slot,false);
    return option::none;    
}


Option<itl_error> compile_switch_block(Interloper& itl,Function& func, AstNode* node)
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
        return compile_error(itl,itl_error::missing_args,"Switch statement has no cases");
    }

    CaseNode* first_case = (CaseNode*)switch_node->statements[0];
    
    // collapse all the values of the statements now we have parsed everything
    // NOTE: we cant do this as the ast is built, as we want to allow usage of values that are defined "out of order"
    switch(first_case->statement->type)
    {
        // integer expression
        default:
        {
            assert(false);
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
        return compile_error(itl,itl_error::undeclared,"switch on enum %s missing cases:",enumeration->name.buf);    
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
            return compile_error(itl,itl_error::redeclaration,"duplicate case %d",switch_node->statements[i]->value);
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
            return switch_res.error();
        }

        const auto switch_reg = *switch_res;

        // type check the switch stmt
        switch(switch_type)
        {
            case switch_kind::integer:
            {
                if(!is_integer(switch_reg.type))
                {
                    return compile_error(itl,itl_error::int_type_error,"expected integer for switch statement got %s",type_name(itl,switch_reg.type).buf);
                }
                break;
            }

            case switch_kind::enum_t:
            {
                if(is_enum(switch_reg.type))
                {
                    EnumType* enum_type = (EnumType*)switch_reg.type;
                    if(enum_type->enum_idx != enumeration->type_idx)
                    {
                        return compile_error(itl,itl_error::enum_type_error,"expected enum of type %s got %s",
                            enumeration->name.buf,type_name(itl,switch_reg.type));                      
                    }
                }

                else
                {
                    return compile_error(itl,itl_error::enum_type_error,"expected enum of type %s got %s",
                        enumeration->name.buf,type_name(itl,switch_reg.type));                   
                }
                break;
            }
        }

        // save our cur block so we can emit the default block dispatch later
        const BlockSlot range_block = cur_block(func);

        // finally emit the dispatch on the table now we know where to exit if the table bounds get execeeded
        const RegSlot switch_slot = new_tmp(func,GPR_SIZE);
        sub_imm(itl,func,switch_slot,switch_reg.slot,min);

        const RegSlot default_cmp = new_tmp(func,GPR_SIZE);
        cmp_unsigned_gt_imm(itl,func,default_cmp,switch_slot,max - min);

        // NOTE: branch is emitted later as we dont know where it goes yet

        // emit the switch table dispatch
        const BlockSlot dispatch_block = new_basic_block(itl,func);


        // reserve space for the table inside the constant pool
        const PoolSlot pool_slot = reserve_const_pool_section(itl.const_pool,pool_type::jump_table,GPR_SIZE * range);
        const RegSlot table_addr = pool_addr_res(itl,func,pool_slot,0);

        const auto addr = generate_indexed_pointer(itl,func,table_addr,switch_slot,GPR_SIZE,0);

        // load the address out of the jump table
        const RegSlot target = load_addr_gpr_res(itl,func,addr);

        // branch on it
        branch_reg(itl,func,target);

        // finally compile all the blocks, and populate the jump table

        // compile each stmt block
        for(u32 i = 0; i < size; i++)
        {
            CaseNode* case_node = switch_node->statements[i];

            const auto case_slot_res = compile_basic_block(itl,func,case_node->block);
            if(!case_slot_res)
            {
                return case_slot_res.error();
            }

            const BlockSlot case_slot = *case_slot_res;

            // add link from dispatch to case
            add_block_exit(func,dispatch_block,case_slot);

            const Block& case_block = block_from_slot(func,case_slot);
            case_node->label = case_block.label_slot;

            case_node->end_block = cur_block(func);
        }




        // NOTE: as default is allways the last block it does not need to have a jump to the exit
        BlockSlot default_block = {INVALID_BLOCK_HANDLE};


        // if there is no default then our exit label is the end
        if(switch_node->default_statement)
        {
            const auto default_block_res = compile_basic_block(itl,func,(BlockNode*)switch_node->default_statement->next);
            if(!default_block_res)
            {
                return default_block_res.error();
            }

            default_block = *default_block_res;

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

    return option::none;
}
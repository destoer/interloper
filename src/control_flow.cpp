

void compile_if_block(Interloper &itl,Function &func,AstNode *node)
{
    BlockNode* if_block = (BlockNode*)node;

    BlockSlot start_block = cur_block(func);
    BlockSlot exit_block;

    for(u32 n = 0; n < count(if_block->statements); n++)
    {
        BinNode* if_stmt = (BinNode*)if_block->statements[n];

        // compile the compare expr if conditon
        auto [t,r] = compile_oper(itl,func,if_stmt->left);

        // integer or pointer is fine check they aernt zero as a shorthand
        if(is_integer(t) || is_pointer(t))
        {
            r = cmp_ne_imm_res(itl,func,r,0);
        }
        
        else if(!is_bool(t))
        {
            panic(itl,itl_error::bool_type_error,"expected bool got %s in if condition\n",type_name(itl,t).buf);
            return;
        }

        // block for comparison branch
        // cant emit yet as we dont know how may blocks the if statement we are jumping over is yet
        const BlockSlot cmp_block = cur_block(func);

        // compile the body block
        const BlockSlot body_slot = compile_basic_block(itl,func,(BlockNode*)if_stmt->right);

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
                const BlockSlot else_slot = compile_basic_block(itl,func,(BlockNode*)else_stmt->next);

                // add branch over body we compiled to else statement
                emit_cond_branch(func,cmp_block,else_slot,body_slot,r,false);

                exit_block = add_fall(itl,func);
                break;
            }

            else
            {
                // create new block for compare for the next node
                // TODO: should this have hte type of the initial node or no?
                const BlockSlot chain_slot = new_basic_block(itl,func);

                // add branch over the body we compiled earlier
                emit_cond_branch(func,cmp_block,chain_slot,body_slot,r,false);
            }
        }

        // Final block, this exit is done via a fallthrough
        else
        {
            exit_block = add_fall(itl,func);

            // if cond not met just branch into exit block
            emit_cond_branch(func,cmp_block,exit_block,body_slot,r,false);          
        }
    }

    if(itl.error)
    {
        return;
    }

    // now we have to exit block add in every exit
    auto &blocks = func.emitter.program;

    for(u32 b = start_block.handle; b < count(blocks) - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.list.end)
        {
            if(block.list.end->opcode.op == op_type::exit_block)
            {
                remove(block.list,block.list.end);
                emit_branch(func,block_from_idx(b),exit_block);
            }
        }
    }
}

void compile_while_block(Interloper &itl,Function &func,AstNode *node)
{
    const BlockSlot initial_block = cur_block(func);

    BinNode* while_node = (BinNode*)node;

    // compile cond
    auto [cond_type,entry_cond] = compile_oper(itl,func,while_node->left);

    // integer or pointer, check not zero
    if(is_integer(cond_type) || is_pointer(cond_type))
    {
        entry_cond = cmp_ne_imm_res(itl,func,entry_cond,0);
    }

    // check cond is actually a bool
    else if(!is_bool(cond_type))
    {
        panic(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,cond_type).buf);
        return;
    }    

    // compile body
    const BlockSlot while_block = compile_basic_block(itl,func,(BlockNode*)while_node->right); 

    const BlockSlot end_block = cur_block(func);

    SymSlot exit_cond;
    std::tie(std::ignore,exit_cond) = compile_oper(itl,func,while_node->left);

    // integer or pointer, check not zero
    if(is_integer(cond_type) || is_pointer(cond_type))
    {
        exit_cond = cmp_ne_imm_res(itl,func,exit_cond,0);
    }

    const BlockSlot exit_block = new_basic_block(itl,func);

    // keep looping to while block if cond is true
    emit_cond_branch(func,end_block,while_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,while_block,entry_cond,false);   
}

void compile_for_range_idx(Interloper& itl, Function& func, ForRangeNode* for_node, b32 inc, op_type cmp_type)
{
    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    BinNode* cmp_node = (BinNode*)for_node->cond;

    // grab end
    // NOTE: we need to regrab this later incase it is not a const
    const auto [entry_end_type,entry_end] = compile_oper(itl,func,cmp_node->right);

    // make index the same sign as the end stmt
    const auto& sym = add_symbol(itl,for_node->name_one,make_builtin(itl,inc? builtin_type::u32_t : builtin_type::s32_t)); 

    const SymSlot index = sym.reg.slot;

    // grab initalizer
    const auto entry_init_type = compile_expression(itl,func,cmp_node->left,index);

    if(!is_integer(entry_init_type) || !is_integer(entry_end_type))
    {
        panic(itl,itl_error::bool_type_error,"expected integer's in range conditon got %s,%s\n",
            type_name(itl,entry_init_type).buf,type_name(itl,entry_init_type).buf);
        return;
    }    

    // compile in cmp for entry
    SymSlot entry_cond = new_tmp(func,GPR_SIZE);
    emit_block_internal_slot(func,initial_block,cmp_type,entry_cond,index,entry_end);

    // compile the main loop body
    const BlockSlot for_block = compile_basic_block(itl,func,for_node->block); 

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
    const auto [exit_end_type,exit_end] = compile_oper(itl,func,cmp_node->right);

    SymSlot exit_cond = new_tmp(func,GPR_SIZE);
    emit_block_internal_slot(func,end_block,cmp_type,exit_cond,index,exit_end);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,for_block,entry_cond,false);    
}

void compile_for_range_arr(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    const auto [type, arr_slot] = compile_expression_tmp(itl,func,for_node->cond);

    if(!is_array(type))
    {
        panic(itl,itl_error::array_type_error,"Expected array for range stmt got %s\n",type_name(itl,type).buf);
        return;
    }

    ArrayType* arr_type = (ArrayType*)type;

    const u32 index_size = arr_type->sub_size;

    // attempting to index statically zero array
    // we dont care
    if(is_fixed_array(arr_type) && arr_type->size == 0)
    {
        return;
    }

    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    const b32 track_idx = for_node->name_two.buf != nullptr;
    SymSlot index = SYM_ERROR;

    SymSlot data = SYM_ERROR;

    // create var to hold data
    if(!for_node->take_pointer)
    {
        const auto& sym = add_symbol(itl,for_node->name_one,index_arr(arr_type));
        data = sym.reg.slot;
    }

    else
    {
        const auto& sym = add_symbol(itl,for_node->name_one,make_pointer(itl,index_arr(arr_type)));
        data = sym.reg.slot;
    }
    

    if(track_idx)
    {
        // check our loop index does not exist
        if(symbol_exists(itl.symbol_table,for_node->name_two))
        {
            panic(itl,itl_error::redeclaration,"redeclared index in for range loop: %s\n",for_node->name_one.buf);
            return;
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

    SymSlot entry_cond = {SYMBOL_NO_SLOT};

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
        do_ptr_load(itl,func,data,arr_data,arr_type->contained_type);
    }

    // move the pointer in the data
    else
    {
        mov_reg(itl,func,data,arr_data);
    }

    compile_block(itl,func,for_node->block);

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
    emit_cond_branch(func,end_block,for_block,exit_block,exit_cond,true);

    if(is_runtime_size(arr_type))
    {
        // emit branch over the loop body if array is empty
        emit_cond_branch(func,initial_block,exit_block,for_block,entry_cond,false);  
    }

    // fixed size array only need to add a fall
    // as we know its not empty by this point
    else
    {
        add_block_exit(func,initial_block,for_block);
    }
}


void compile_for_range(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    // scope for any var decls in the stmt
    new_anon_scope(itl.symbol_table);



    // check our loop index does not exist
    if(symbol_exists(itl.symbol_table,for_node->name_one))
    {
        panic(itl,itl_error::redeclaration,"redeclared index in for range loop: %s\n",for_node->name_one.buf);
        return;
    }

    // determine what kind of loop term we have
    switch(for_node->cond->type)
    {
        // <= or < is inc
        case ast_type::logical_lt:
        {
            compile_for_range_idx(itl,func,for_node,true,op_type::cmpslt_reg);
            break;
        }

        case ast_type::logical_le:
        {
            compile_for_range_idx(itl,func,for_node,true,op_type::cmpsle_reg);
            break;
        }

        // >= or > is dec
        case ast_type::logical_gt:
        {
            compile_for_range_idx(itl,func,for_node,false,op_type::cmpsgt_reg);
            break;
        }

        case ast_type::logical_ge:
        {
            compile_for_range_idx(itl,func,for_node,false,op_type::cmpsge_reg);
            break;
        }

        // array index!
        default:
        {
            compile_for_range_arr(itl,func,for_node);
            break;
        }
    }

    destroy_scope(itl.symbol_table);
}

void compile_for_iter(Interloper& itl, Function& func, ForIterNode* for_node)
{
    // scope for any var decls in the stmt
    new_anon_scope(itl.symbol_table);

    const BlockSlot initial_block = cur_block(func);
  

   // compile the first stmt (ussualy an assign)

    
    const auto type = for_node->initializer->type;

    // handle this being a declaration
    switch(type)
    {
        case ast_type::auto_decl:
        {
            compile_auto_decl(itl,func,for_node->initializer);
            break;
        }

        case ast_type::declaration:
        {
            compile_decl(itl,func,for_node->initializer);
            break;
        }

        default:
        {
            compile_expression_tmp(itl,func,for_node->initializer);
            break;
        }
    }

    if(itl.error)
    {
        return;
    }
    
    // compile cond for entry and check it is a bool
    const auto [cond_type,entry_cond] = compile_oper(itl,func,for_node->cond);

    if(!is_bool(cond_type))
    {
        panic(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,cond_type).buf);
        return;
    }    


    // compile the body
    const BlockSlot for_block = compile_basic_block(itl,func,for_node->block);    

    // compile loop end stmt
    compile_expression_tmp(itl,func,for_node->post);
    
    if(itl.error)
    {
        return;
    }

    const BlockSlot end_block = cur_block(func);


    SymSlot exit_cond;
    std::tie(std::ignore,exit_cond) = compile_oper(itl,func,for_node->cond);

    const BlockSlot exit_block = new_basic_block(itl,func);

    emit_cond_branch(func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,for_block,entry_cond,false);

    destroy_scope(itl.symbol_table);    
}


void compile_switch_block(Interloper& itl,Function& func, AstNode* node)
{
    SwitchNode* switch_node = (SwitchNode*)node;


    const u32 size = count(switch_node->statements);


    enum class switch_kind
    {
        integer,
        enum_t,
    };

    switch_kind switch_type;

    // used for checking enums match
    u32 type_idx = 0;
    

    if(size == 0)
    {
        panic(itl,itl_error::missing_args,"Switch statement has no cases");
        return;
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
            // TODO: First part of scope is just presumed as the enum this is probably technically wrong
            TypeDecl* type_decl = lookup_type(itl,first_stmt->scope[0]);

            if(!(type_decl && type_decl->kind == type_kind::enum_t))
            {
                // case is not an enum
                panic(itl,itl_error::enum_type_error,"case is not an emum %s\n",first_stmt->scope[0].buf);
                return;
            }

            type_idx = type_decl->type_idx;
            Enum& enumeration = itl.enum_table[type_idx];


            for(u32 i = 0; i < size; i++)
            {
                CaseNode* case_node = switch_node->statements[i];


                // check we have a matching enum
                if(case_node->statement->type != ast_type::scope)
                {
                    panic(itl,itl_error::enum_type_error,"switch: one or more cases are not an enum\n");
                    return;
                }

                ScopeNode* scope_node = (ScopeNode*)case_node->statement;

                if(first_stmt->scope[0] != scope_node->scope[0])
                {
                    panic(itl,itl_error::enum_type_error,"differing enums %s : %s in switch statement\n",first_stmt->scope[0].buf,scope_node->scope[0].buf);
                    return;
                }


                // pull the member value
                LiteralNode *member_node = (LiteralNode*)scope_node->expr;
                EnumMember* enum_member = lookup(enumeration.member_map,member_node->literal);

                if(!enum_member)
                {
                    panic(itl,itl_error::enum_type_error,"enum %s no such member %s\n",scope_node->scope[0].buf,member_node->literal);
                    return;
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

                const auto [case_value,case_type] = compile_const_int_expression(itl,case_node->statement); 
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
    if(switch_type == switch_kind::enum_t && !switch_node->default_statement && size != itl.enum_table[type_idx].member_map.size)
    {
        // TODO: print the missing cases
        panic(itl,itl_error::undeclared,"switch on enum %s missing cases:\n",itl.enum_table[type_idx].name.buf);
        return;     
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
            panic(itl,itl_error::redeclaration,"duplicate case %d\n",switch_node->statements[i]->value);
            return;
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
        const auto [rtype,expr_slot] = compile_oper(itl,func,switch_node->expr);

        // type check the switch stmt
        switch(switch_type)
        {
            case switch_kind::integer:
            {
                if(!is_integer(rtype))
                {
                    panic(itl,itl_error::int_type_error,"expected integer for switch statement got %s\n",type_name(itl,rtype).buf);
                    return;
                }
                break;
            }

            case switch_kind::enum_t:
            {
                if(is_enum(rtype))
                {
                    EnumType* enum_type = (EnumType*)rtype;
                    if(enum_type->enum_idx != type_idx)
                    {
                        panic(itl,itl_error::enum_type_error,"expected enum of type %s got %s\n",itl.enum_table[type_idx].name.buf,type_name(itl,rtype));
                        return;                        
                    }
                }

                else
                {
                    panic(itl,itl_error::enum_type_error,"expected enum of type %s got %s\n",itl.enum_table[type_idx].name.buf,type_name(itl,rtype));
                    return;                    
                }
                break;
            }
        }

        // save our cur block so we can emit the default block dispatch later
        const BlockSlot range_block = cur_block(func);

        // finally emit the dispatch on the table now we know where to exit if the table bounds get execeeded
        const SymSlot switch_slot = new_tmp(func,GPR_SIZE);
        sub_imm(itl,func,switch_slot,expr_slot,min);

        const SymSlot default_cmp = new_tmp(func,GPR_SIZE);
        cmp_unsigned_gt_imm(itl,func,default_cmp,switch_slot,max - min);

        // NOTE: branch is emitted later as we dont know where it goes yet

        // emit the switch table dispatch
        const BlockSlot dispatch_block = new_basic_block(itl,func);

        // mulitply to get a jump table index
        const SymSlot table_index = mul_imm_res(itl,func,switch_slot,GPR_SIZE);

        
        // reserve space for the table inside the constant pool
        const PoolSlot pool_slot = reserve_const_pool_section(itl.const_pool,pool_type::jump_table,GPR_SIZE * range);
        const SymSlot table_addr = pool_addr_res(itl,func,pool_slot,0);

        // get address in the tabel we want
        const SymSlot final_offset = add_res(itl,func,table_addr,table_index);

        // load the address out of the jump table
        const SymSlot target = new_tmp_ptr(func);
        load_ptr(itl,func,target, final_offset,0, GPR_SIZE,false,false);

        // branch on it
        branch_reg(itl,func,target);

        // finally compile all the blocks, and populate the jump table

        // compile each stmt block
        for(u32 i = 0; i < size; i++)
        {
            CaseNode* case_node = switch_node->statements[i];

            const BlockSlot case_slot = compile_basic_block(itl,func,case_node->block);

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
            default_block = compile_basic_block(itl,func,(BlockNode*)switch_node->default_statement->next);
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
        emit_cond_branch(func,range_block,default_block,dispatch_block,default_cmp,true);



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
                emit_branch(func,case_node->end_block,exit_block);
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
}
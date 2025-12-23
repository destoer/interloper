
struct IfCompile
{
    BlockSlot start_block;
    BlockSlot exit_block;
    Option<AstBlock> else_stmt = option::none;
    u32 compiled = 0;
    u32 count = 0;
};

IfCompile make_if_compile(Function &func,IfNode* node)
{
    IfCompile out;
    out.count = node->count;
    out.start_block = cur_block(func);
    if(node->else_clause)
    {
        out.else_stmt = node->else_stmt;
    }

    return out;
}

BlockSlot compile_basic_block(Interloper& itl, Function& func, AstBlock& block)
{
    const BlockSlot block_slot = new_basic_block(itl,func);
    compile_block(itl,func,block);

    return block_slot;
}


void compile_if_stmt(Interloper& itl, Function& func, const IfStmt& stmt, IfCompile* compile)
{
    auto cond = compile_oper(itl,func,stmt.expr);
    switch(stmt.type)
    {
        // Already 1
        case if_stmt_type::bool_t:
        {
            break;
        }

        case if_stmt_type::not_zero_t:
        {
            cond.slot = cmp_ne_imm_res(itl,func,cond.slot,0);
            break;
        }

        case if_stmt_type::array_t:
        {
            const auto len = load_arr_len(itl,func,cond);
            cond.slot = cmp_ne_imm_res(itl,func,len,0);
            break;
        }
    }


    // block for comparison branch
    // cant emit yet as we dont know how may blocks the if statement we are jumping over is yet
    const BlockSlot cmp_block = cur_block(func);
    const BlockSlot body_block = compile_basic_block(itl,func,*stmt.block);
    compile->compiled += 1;


    if(compile->count == compile->compiled)
    {
        compile->exit_block = add_fall(itl,func);

        // if cond not met just branch into exit block
        emit_cond_branch(itl,func,cmp_block,compile->exit_block,body_block,cond.slot,false);
    }

    else
    {
        // indicate we need to jump the exit block
        emit_exit_block(itl,func);

        // About to hit an else stmt we wont have another cond
        if(compile->count == compile->compiled + 1 && compile->else_stmt)
        {
            auto else_stmt = *compile->else_stmt;

            const auto else_block = compile_basic_block(itl,func,else_stmt);
            compile->compiled += 1;

            // add branch over body we compiled to else statement
            emit_cond_branch(itl,func,cmp_block,else_block,body_block,cond.slot,false);

            // By definition this is the last stmt
            compile->exit_block = add_fall(itl,func);
        }

        else
        {
            // create new block for compare for the next node
            const BlockSlot chain_slot = new_basic_block(itl,func);

            // add branch over the body we compiled earlier
            emit_cond_branch(itl,func,cmp_block,chain_slot,body_block,cond.slot,false);
        }
    }
}

void compile_if(Interloper& itl, Function& func, AstNode* node)
{
    IfNode* if_node = (IfNode*)node;

    auto compile = make_if_compile(func,if_node);

    // Compile the initial stmt
    compile_if_stmt(itl,func,if_node->if_stmt,&compile);

    // Compile any else if stmts
    for(auto& else_if_stmt : if_node->else_if_stmt)
    {
        compile_if_stmt(itl,func,else_if_stmt,&compile);
    }

    for(u32 b = compile.start_block.handle; b != compile.exit_block.handle; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.list.finish)
        {
            if(block.list.finish->value.op == op_type::exit_block)
            {
                remove(block.list,block.list.finish);
                emit_branch(itl,func,block.block_slot,compile.exit_block);
            }
        }
    }
}

void compile_range_for_idx(Interloper& itl, Function& func, ForRangeNode* range)
{
    const bool is_inc = (range->flags & RANGE_FOR_INC) == RANGE_FOR_INC;

    const auto cmp_type = (comparison_op)range->cmp_op;


    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    CmpNode* cmp = (CmpNode*)range->cond;

    const auto end = compile_oper(itl,func,cmp->right);
    const auto index = typed_reg(sym_from_slot(itl.symbol_table,range->sym_one.slot));

    const auto sign = is_signed(index.type);

    // grab initializer
    compile_expression(itl,func,cmp->left,index.slot);

    // compile in cmp for entry
    const RegSlot entry_cond = new_tmp(func,GPR_SIZE);
    emit_integer_compare(itl,func,cmp_type,sign,entry_cond,index.slot,end.slot);

    // compile the main loop body
    const auto for_block = compile_basic_block(itl,func,range->block); 

    // compile post inc / dec
    if(is_inc)
    {
        add_imm(itl,func,index.slot,index.slot,1);
    }

    else
    {
        sub_imm(itl,func,index.slot,index.slot,1);
    }

    // compile body check
    const BlockSlot end_block = cur_block(func);

    // regrab end
    const auto exit = compile_oper(itl,func,cmp->right);

    const RegSlot exit_cond = new_tmp(func,GPR_SIZE);
    emit_integer_compare(itl,func,cmp_type,sign,exit_cond,index.slot,exit.slot);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry_cond,false);
}

void compile_range_for_array(Interloper& itl, Function& func, ForRangeNode* range)
{
    const auto entry_arr = compile_expression_tmp(itl,func,range->cond);
    ArrayType* arr_type = (ArrayType*)entry_arr.type;

    const u32 index_size = arr_type->sub_size;

    // attempting to index statically zero array
    // we dont care
    if(is_fixed_array(arr_type) && arr_type->size == 0)
    {
        return;
    }

    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    const b32 track_idx = (range->flags & RANGE_FOR_ARRAY_IDX) == RANGE_FOR_ARRAY_IDX;
    const b32 take_pointer = (range->flags & RANGE_FOR_TAKE_POINTER) == RANGE_FOR_TAKE_POINTER;

    RegSlot data = make_sym_reg_slot(range->sym_one.slot);
    RegSlot index = make_sym_reg_slot(range->sym_two.slot);

    
    if(track_idx)
    {
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
    if(!take_pointer)
    {
        // insert the array load inside the for block
        // before we compile the actual stmts
        const TypedReg load_reg = {arr_data,arr_type->contained_type};
        do_ptr_load(itl,func,data,load_reg);
    }

    // move the pointer in the data
    else
    {
        mov_reg(itl,func,data,arr_data);
    }

    compile_block(itl,func,range->block);

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
}

void compile_range_for(Interloper& itl, Function& func, AstNode* stmt)
{
    ForRangeNode* range = (ForRangeNode*)stmt;

    if(range->flags & RANGE_FOR_ARRAY)
    {
        compile_range_for_array(itl,func,range);
    }

    else
    {
        compile_range_for_idx(itl,func,range);
    }
}


void compile_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    const auto& ast_info = AST_INFO[u32(stmt->type)];
    ast_info.compile_stmt(itl,func,stmt);
}

void compile_for_iter(Interloper& itl, Function& func, AstNode* stmt)
{
    ForIterNode* iter = (ForIterNode*)stmt;

    // Compile init stmt
    compile_stmt(itl,func,iter->initializer);

    const auto entry = compile_oper(itl,func,iter->cond);
    const BlockSlot initial_block = cur_block(func);
  

    // compile the body
    const BlockSlot for_block = compile_basic_block(itl,func,iter->block);    

    compile_stmt(itl,func,iter->post);

    const auto exit = compile_oper(itl,func,iter->cond);

    const BlockSlot end_block = cur_block(func);

    const BlockSlot exit_block = new_basic_block(itl,func);

    emit_cond_branch(itl,func,end_block,for_block,exit_block,exit.slot,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,for_block,entry.slot,false);        
}

void compile_while_node(Interloper& itl, Function& func, AstNode* stmt)
{
    WhileNode* while_node = (WhileNode*)stmt;


    // compile cond
    auto entry_cond = compile_oper(itl,func,while_node->expr);
    const BlockSlot initial_block = cur_block(func);

    if(while_node->cond_type == while_cond_type::not_zero_t)
    {
        entry_cond.slot = cmp_ne_imm_res(itl,func,entry_cond.slot,0);
    }

    // compile body
    const BlockSlot while_block = compile_basic_block(itl,func,while_node->block); 

    auto exit_cond = compile_oper(itl,func,while_node->expr);
    if(while_node->cond_type == while_cond_type::not_zero_t)
    {
        exit_cond.slot = cmp_ne_imm_res(itl,func,exit_cond.slot,0);
    }

    const BlockSlot end_block = cur_block(func);

    const BlockSlot exit_block = new_basic_block(itl,func);

    // keep looping to while block if cond is true
    emit_cond_branch(itl,func,end_block,while_block,exit_block,exit_cond.slot,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(itl,func,initial_block,exit_block,while_block,entry_cond.slot,false); 
}
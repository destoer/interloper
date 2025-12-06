
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
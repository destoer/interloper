#include <interloper.h>
#include "expression.cpp"

void type_panic(Parser &parser);
AstNode *block(Parser &parser);


// TODO: replace peek(parser,0); with match()

// TODO: replace tree with pool allocation
void delete_tree(AstNode *node)
{
    if(!node)
    {
        return;
    }


    for(auto &n: node->nodes)
    {
        delete_tree(n);
        n = nullptr;
    }

    delete node;
}


Token next_token(Parser &parser)
{
    if(parser.tok_idx >= parser.tokens.size())
    {
        // TODO: make this return the actual file end
        // for row and col
        return Token(token_type::eof,"",0,0);
    }

    return parser.tokens[parser.tok_idx++];  
}

void prev_token(Parser &parser)
{
    if(parser.tok_idx != 0)
    {
        parser.tok_idx -= 1;
    }
}


Token peek(Parser &parser,u32 v)
{
    const auto idx = parser.tok_idx + v;
    if(idx >= parser.tokens.size())
    {
        return Token(token_type::eof,"",0,0);
    }

    return parser.tokens[idx];
}



Value read_value(const Token &t)
{
    const u32 v = convert_imm(t.literal); 

    // is this literal a -ve?
    const bool sign = t.literal[0] == '-';

    return Value(v,sign);
}

void consume(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= parser.tokens.size()? token_type::eof : parser.tokens[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        panic(parser,tok,"expected %s got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
}

bool match(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= parser.tokens.size()? token_type::eof : parser.tokens[parser.tok_idx].type;

    return t == type;
}


AstNode *copy_node(const AstNode *node)
{
    if(!node)
    {
        return nullptr;
    }

    auto copy  = alloc_node();

    copy->type = node->type;
    copy->literal = node->literal;

    switch(copy->type)
    {
        case ast_type::value:
        {
            copy->value = node->value;
            break;
        }

        case ast_type::ptr_indirection:
        case ast_type::type:
        {
            copy->type_idx = node->type_idx;
        }

        default: break;
    }

    for(const AstNode* n : node->nodes)
    {
        if(n)
        {
            copy->nodes.push_back(copy_node(n));
        }
    }

    return copy;
}


void type_panic(Parser &parser)
{
    const auto tok = peek(parser,1);
    panic(parser,tok,"expected type declaration got: %s\n",tok_name(tok.type));
}


bool is_builtin_type_tok(const Token &tok)
{
   return tok.type >= token_type::u8 && tok.type <= token_type::bool_t; 
}

u32 plain_type_idx(const Token &tok)
{
    // within the plain type range
    if(is_builtin_type_tok(tok))
    {
        // compute builtin type idx 
        return s32(tok.type) - s32(token_type::u8);
    }

    // we might not know what this is yet so we will resolve the idx properly later...
    else if(tok.type == token_type::symbol)
    {
        return STRUCT_IDX;
    }

    else
    {
        return INVALID_TYPE;
    }    
}


AstNode *parse_type(Parser &parser)
{
    // parse out any specifiers
    auto specifier = peek(parser,0);

    b32 is_const = false;

    switch(specifier.type)
    {
        case token_type::const_t:
        {
            next_token(parser);
            is_const = true;
            break;
        }


        // no specifier
        default: break;
    }

    // read out the plain type

    auto plain_tok = next_token(parser);

    u32 type_idx = plain_type_idx(plain_tok);

    // null cannot be obtained via normal means
    if(type_idx == INVALID_TYPE || type_idx == u32(builtin_type::null_t))
    {
        panic(parser,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
        return nullptr;
    }

    std::string type_literal;
    
    if(type_idx == STRUCT_IDX)
    {   
        type_literal = plain_tok.literal;
    }

    else
    {
        type_literal = TYPE_NAMES[type_idx];
    }

    // TODO: need to mark the idx for when have user defined types
    auto type = ast_plain(ast_type::type);
    type->type_idx = type_idx;
    

    if(is_const)
    {
        type->nodes.push_back(ast_plain(ast_type::const_t));
    }

    b32 quit = false;

    while(!quit)
    {
        // parse out other types, arrays, pointers
        switch(peek(parser,0).type)
        {
            // pointer decl
            case token_type::deref:
            {
                u32 ptr_indirection = 0;
                while(peek(parser,0).type == token_type::deref)
                {
                    next_token(parser);
                    ptr_indirection++;
                }

                auto ptr_node = ast_plain(ast_type::ptr_indirection);
                ptr_node->type_idx = ptr_indirection;

                ptr_node->literal = std::to_string(ptr_indirection);

                type->nodes.push_back(ptr_node);
                break;
            }


            // array decl
            case token_type::sl_brace:
            {
                auto arr_decl = ast_plain(ast_type::arr_dimensions);

                while(peek(parser,0).type == token_type::sl_brace)
                {
                    consume(parser,token_type::sl_brace);

                    // var size
                    if(peek(parser,0).type == token_type::sr_brace)
                    {
                        arr_decl->nodes.push_back(ast_plain(ast_type::arr_var_size));
                        consume(parser,token_type::sr_brace);
                    }

                    else 
                    {
                        // figure out this size later
                        if(peek(parser,0).type == token_type::qmark)
                        {
                            consume(parser,token_type::qmark);

                            const auto e = ast_plain(ast_type::arr_deduce_size);
                            arr_decl->nodes.push_back(e);
                        
                            consume(parser,token_type::sr_brace);
                        }

                        else
                        {
                            arr_decl->nodes.push_back(expr_terminate(parser,token_type::sr_brace));
                        }
                    }
                }

                type->nodes.push_back(arr_decl);

                break;
            }

            default: quit = true; break;
        }
    }

    type->literal = type_literal;

    return type;
}




AstNode *declaration(Parser &parser)
{
    // declartion
    // symbol ':' type ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: '%s'  (%zd)\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    consume(parser,token_type::colon);


    AstNode* type = parse_type(parser);

    if(!type)
    {
        type_panic(parser);
        return nullptr;
    }

    //    [declare:name]
    // [type]   optional([eqauls])

    auto d = ast_literal(ast_type::declaration,s.literal);
    d->nodes.push_back(type);

    const auto eq = peek(parser,0);

    switch(eq.type)
    {
        // declartion without assigment
        case token_type::semi_colon:
        {
            consume(parser,token_type::semi_colon);
            break;
        }

        // declartion with assingment
        case token_type::equal:
        {
            consume(parser,token_type::equal);
            
            const auto e = expr(parser,next_token(parser));
            
            // declartion with initalizer skip over initial symbol
            if(e)
            {
                d->nodes.push_back(e);
            }
            break;
        }

        default:
        {
            delete_tree(d);
            panic(parser,eq,"malformed declartion: %s\n",tok_name(eq.type));
            break;
        }
    }

    return d;
}

AstNode *auto_decl(Parser &parser)
{
    // decl symbol = expr;
    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: %s:%zd\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    
    // okay here we require an expression on the right side
    consume(parser,token_type::equal);

    const auto d = ast_literal(ast_type::auto_decl,s.literal);

    d->nodes.push_back(expr(parser,next_token(parser)));

    return d;    
}


AstNode *statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        case token_type::decl:
        {
            return auto_decl(parser);
        }

        case token_type::ret:
        {
            auto r = ast_plain(ast_type::ret);

            // return value is optional
            if(peek(parser,0) != token_type::semi_colon)
            {
                r->nodes.push_back(expr(parser,next_token(parser)));
            }
            else
            {
                consume(parser,token_type::semi_colon);
            }

            return r;
        }


        case token_type::deref:
        {
            const auto t2 = peek(parser,0);

            if(t2.type != token_type::symbol)
            {
                panic(parser,t2,"statement: expected symbol for deref %s\n");
                break;
            }

            return expr(parser,t);
        }

        // TODO: detect a declartion with a user defined type
        case token_type::symbol:
        {
            const auto t2 = peek(parser,0);

            switch(t2.type)
            {
                // declaration with specified type
                case token_type::colon:
                {
                    prev_token(parser);
                    return declaration(parser);    
                }

                // assignment expr
                case token_type::plus_eq:
                case token_type::minus_eq:
                case token_type::divide_eq:
                case token_type::times_eq:
                case token_type::equal:
                {
                    return expr(parser,t);
                }

                // check for brackets
                // array indexes etc here 

                // function call
                case token_type::left_paren:
                {
                    return expr(parser,t);
                }

                // array access
                case token_type::sl_brace:
                {
                    return expr(parser,t);
                }

                // expr for member access?
                case token_type::dot:
                {
                    return expr(parser,t);
                }


                default:
                {
                    panic(parser,t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                    break;
                }
            }
            break;
        }

        case token_type::left_c_brace:
        {
            // block expects to see the left c brace
            parser.tok_idx--;

            return block(parser);
        }

        // assume one cond for now
        case token_type::for_t:
        {
            const auto for_block = ast_plain(ast_type::for_block);

            // allow statment to wrapped a in a set of parens
            const bool term_paren = peek(parser,0).type == token_type::left_paren;
            const token_type first_term = term_paren? token_type::right_paren : token_type::left_c_brace;
            token_type terminator;

            // ignore the first paren
            if(term_paren)
            {
                consume(parser,token_type::left_paren);
            }

            // auto decl
            if(peek(parser,0).type == token_type::decl)
            {
                consume(parser,token_type::decl);
                for_block->nodes.push_back(auto_decl(parser));
                terminator = token_type::semi_colon;                 
            }

            // standard decl
            else
            {
                // decl 
                if(peek(parser,1).type == token_type::colon)
                {
                    for_block->nodes.push_back(declaration(parser));
                    terminator = token_type::semi_colon; 
                }

                // standard stmt
                else
                {
                    for_block->nodes.push_back(expr_terminate(parser,first_term,terminator)); 
                }
            }


            // for(s32 x = 5; x > 0; x -= 1) (multiple statement)
            if(terminator == token_type::semi_colon)
            {
                for_block->nodes.push_back(expr_terminate(parser,token_type::semi_colon)); 

                // allow paren terminator followed by a '{'
                if(term_paren)
                {
                    for_block->nodes.push_back(expr_terminate(parser,token_type::right_paren));
                    auto next = peek(parser,0);
                    if(next.type != token_type::left_c_brace)
                    {
                        panic(parser,next,"invalid single for statement terminator: %s\n",tok_name(next.type));
                        return nullptr;                        
                    }
                }

                // statement was not wrapped by parens
                // expect brace to end it
                else
                {
                    for_block->nodes.push_back(expr_terminate(parser,token_type::left_c_brace));
                    prev_token(parser);
                }  
            }

             // for(x > 0) (single statement)
            else
            {
                // allow paren terminator
                if(terminator == token_type::right_paren && term_paren)
                {
                    terminator = peek(parser,0).type;
                }

                if(terminator != token_type::left_c_brace)
                {
                    panic(parser,peek(parser,0),"invalid single if statement terminator: %s\n",tok_name(terminator));
                    return nullptr;                        
                }
            }


            // for stmt parsed now compile the actual block
            for_block->nodes.push_back(block(parser));

            return for_block;
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            const auto if_block = ast_plain(ast_type::if_block);

            const auto if_stmt = ast_plain(ast_type::if_t);

            // read out if expr and block
            if_stmt->nodes.push_back(expr_terminate(parser,token_type::left_c_brace)); prev_token(parser); 
            if_stmt->nodes.push_back(block(parser));

           
            if_block->nodes.push_back(if_stmt);
            
            bool done = false;
            
            while(!done)
            {
                if(peek(parser,0).type == token_type::else_t)
                {
                    consume(parser,token_type::else_t);

                    // we have an else if
                    if(peek(parser,0).type == token_type::if_t)
                    {
                        consume(parser,token_type::if_t);

                        const auto else_if_stmt = ast_plain(ast_type::else_if_t);

                        else_if_stmt->nodes.push_back(expr_terminate(parser,token_type::left_c_brace)); prev_token(parser); 
                        else_if_stmt->nodes.push_back(block(parser));

                        if_block->nodes.push_back(else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        const auto else_stmt = ast_plain(ast_type::else_t);

                        // no expr for else
                        else_stmt->nodes.push_back(block(parser));

                        if_block->nodes.push_back(else_stmt);

                        done = true;
                    }
                }

                // this chain is done we have another token
                else
                {
                    done = true;
                }
            }
            return if_block;
        }

        // dont care
        case token_type::semi_colon:
        {
            break;
        }

        default:
        {
            panic(parser,t,"statement: unexpected token %s\n",tok_name(t.type));
            break;
        }
    }

    return nullptr;
}

AstNode *block(Parser &parser)
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(parser,0);
    consume(parser,token_type::left_c_brace);

    auto b = ast_plain(ast_type::block);

    
    // parse out all our statements
    while(!match(parser,token_type::right_c_brace))
    {
        if(match(parser,token_type::eof))
        {
            delete_tree(b);
            panic(parser,tok,"unterminated block!");
            return nullptr;
        }

        b->nodes.push_back(statement(parser));

        if(parser.error)
        {
            return b;
        }
    }
    

    consume(parser,token_type::right_c_brace);

    return b;
}

AstNode *func(Parser &parser)
{

    // func_dec = func ident(arg...) return_type 
    // arg = ident : type,

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        panic(parser,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return nullptr;  
    }

    AstNode *f = ast_literal(ast_type::function, func_name.literal);

    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);

    auto a = ast_plain(ast_type::function_args);

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            delete_tree(a);
            delete_tree(f);
            panic(parser,paren,"unterminated function declaration!\n");
            return nullptr;
        }

        // for each arg pull type, name
       

        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            delete_tree(a);
            delete_tree(f);
            panic(parser,lit_tok,"expected name for function arg\n");
            return nullptr;
        }
        

        consume(parser,token_type::colon);

        AstNode* type = parse_type(parser);

        if(!type)
        {
            type_panic(parser);
            delete_tree(a);
            delete_tree(f);
            return nullptr;
        }


        // add each declartion
        auto d = ast_literal(ast_type::declaration,lit_tok.literal);
        d->nodes.push_back(type);

        a->nodes.push_back(d);

        // if the declaration isnt closed get the next arg
        if(peek(parser,0).type != token_type::right_paren)
        {
            consume(parser,token_type::comma);
        }
    }

    consume(parser,token_type::right_paren);

    AstNode* return_type;

    if(!match(parser,token_type::left_c_brace))
    {
        return_type = parse_type(parser);

        if(!return_type)
        {
            type_panic(parser);
            return nullptr;
        }
    }

    // void
    else
    {
        return_type = ast_plain(ast_type::type);
        return_type->type_idx = u32(builtin_type::void_t);
        return_type->literal = "void";
    }

    AstNode *b = block(parser); 

    //      [func: name]
    // [type] [block]  [args]
    f->nodes.push_back(return_type);
    f->nodes.push_back(b);
    f->nodes.push_back(a);

    return f;
}

void struct_decl(Interloper& itl,Parser& parser)
{
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        panic(parser,name,"expected name after struct decl got %s\n",tok_name(name.type));
        return;
    }

    if(itl.struct_def.count(name.literal))
    {
        panic(itl,"struct %s redeclared\n",name.literal.c_str());
        return;
    }

    AstNode* struct_node = ast_literal(ast_type::struct_t,name.literal);

    consume(parser,token_type::left_c_brace);

    while(!match(parser,token_type::right_c_brace))
    {
        AstNode* decl = declaration(parser);

        if(!decl)
        {
            panic(parser,name,"malformed struct member decl\n");
            return;
        }

        struct_node->nodes.push_back(decl);
    }

    consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        consume(parser,token_type::semi_colon);
    }

    // TODO: we now should check redefiniton here?
    StructDef definition = {struct_state::not_checked,struct_node,0};

    itl.struct_def[name.literal] = definition;
}

const u32 AST_ALLOC_DEFAULT_SIZE = 1 * 1024 * 1024;

std::vector<std::string> read_source_file(const std::string& filename)
{
    const std::vector<std::string> lines = read_string_lines(read_file(filename));
    if(!lines.size())
    {
        printf("no such file: %s\n",filename.c_str());
        exit(0);
    }    

    return lines;
}


void add_file(std::set<std::string>& file_set, std::vector<std::string>& stack, const std::string& filename)
{
    if(!file_set.count(filename))
    {
        file_set.insert(filename);
        stack.push_back(filename);
    }
}

bool parse(Interloper& itl, const std::string initial_filename)
{
    //print_tokens(parser.tokens);
    std::set<std::string> file_set;
    std::vector<std::string> file_stack;

    add_file(file_set,file_stack,initial_filename);

    // TODO: this should probably be a SHELL VAR but just hard code it for now
    const std::string stl_path = std::string("stl") + std::string(1,path_separator);

    while(file_stack.size())
    {
        // get the next filename to parse
        const auto filename = file_stack.back(); file_stack.pop_back();

        // Parse out the file
        Parser parser;


        auto lines = read_source_file(filename);

        if(tokenize(lines,parser.tokens))
        {
            printf("failed to tokenize file: %s\n",filename.c_str());
            return true;
        }
        
        const auto size = parser.tokens.size();

        while(parser.tok_idx < size)
        {
            const auto &t = next_token(parser);

            // okay what is our "top level" token
            switch(t.type)
            { 
                case token_type::import:
                {
                    if(!match(parser,token_type::string))
                    {
                        panic(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(t.type),t.literal.c_str());
                        return true;
                    }

                    const auto name_tok = next_token(parser);

                    // stl file
                    if(!contains(name_tok.literal,"."))
                    {
                        add_file(file_set,file_stack,get_program_name(stl_path + name_tok.literal));
                    }

                    else
                    {
                        add_file(file_set,file_stack,name_tok.literal);
                    }
                    break;
                }

                // function declartion
                case token_type::func:
                {
                    itl.func_root->nodes.push_back(func(parser));
                    break;
                }

                case token_type::struct_t:
                {

                    struct_decl(itl,parser);
                    break;
                }

                default:
                {
                    panic(parser,t,"unexpected top level token %s: %s\n",tok_name(t.type),t.literal.c_str());
                    break;
                }
            }

            if(parser.error)
            {
                // print line number
                printf("%s\n",lines[parser.line].c_str());
                return true;
            }
        }
    }

    return false;
}

void print(const AstNode *root)
{
    if(!root)
    {
        return;
    }

    if(root->type == ast_type::function || root->type == ast_type::struct_t)
    {
        printf("\n\n\n");
    }


    static int depth = 0;
    
    for(int i = 0; i < depth; i++)
    { 
        printf(" -");
    }
    printf(" %d ",depth);
    
    printf(" %s : %s\n",AST_NAMES[static_cast<size_t>(root->type)],root->literal.c_str());
    depth += 1;

    for(const auto &n: root->nodes)
    {
        print(n);
    }

    depth -= 1;
}
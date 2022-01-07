#include <interloper.h>



std::optional<Type> get_type(Parser &parser,std::string &type_literal);
void type_panic(Parser &parser);
AstNode *block(Parser &parser);


Token next_token(Parser &parser)
{
    const auto &vt = *parser.tokens;
    if(parser.tok_idx >= vt.size())
    {
        // TODO: make this return the actual file end
        // for row and col
        return Token(token_type::eof,"",0,0);
    }

    return vt[parser.tok_idx++];  
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
    const auto &vt = *parser.tokens;
    if(idx >= vt.size())
    {
        return Token(token_type::eof,"",0,0);
    }

    return vt[idx];
}



void consume(Parser &parser,token_type type)
{
    const auto &vt = *parser.tokens;
    const auto t = parser.tok_idx >= vt.size()? token_type::eof : vt[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        panic(parser,tok,"expected %s got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
}

bool match(Parser &parser,token_type type)
{
    const auto &vt = *parser.tokens;
    const auto t = parser.tok_idx >= vt.size()? token_type::eof : vt[parser.tok_idx].type;

    return t == type;
}


AstNode *copy_node(const AstNode *node)
{
    if(!node)
    {
        return nullptr;
    }

    auto copy  = new AstNode();

    copy->type = node->type;
    copy->literal = node->literal;

    switch(copy->type)
    {
        case ast_type::type:
        {
            memcpy(&copy->variable_type,&node->variable_type,sizeof(Type));
            break;
        }

        case ast_type::value:
        {
            memcpy(&copy->value,&node->value,sizeof(Value));
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

std::optional<Type> get_plain_type(const Token &tok, std::string &type_literal)
{
    // is a builtin type
    switch(tok.type)
    {
        case token_type::u8:
        {
            type_literal = "u8";
            return std::optional<Type>(Type(builtin_type::u8_t));
        }

        case token_type::u16:
        {
            type_literal = "u16";
            return std::optional<Type>(Type(builtin_type::u16_t));
        }


        case token_type::u32:
        {
            type_literal = "u32";
            return std::optional<Type>(Type(builtin_type::u32_t));
        }


        case token_type::s8:
        {
            type_literal = "s8";
            return std::optional<Type>(Type(builtin_type::s8_t));
        }

        case token_type::s16:
        {
            type_literal = "s16";
            return std::optional<Type>(Type(builtin_type::s16_t));
        }

        case token_type::s32:
        {
            type_literal = "s32";
            return std::optional<Type>(Type(builtin_type::s32_t));
        }

        case token_type::bool_t:
        {
            type_literal = "bool";
            return std::optional<Type>(Type(builtin_type::bool_t));
        }
        

        case token_type::symbol: // user defined type (must be resolved in initial parser over complete tree)
        {
            // for now just error
            return std::nullopt;
        }

        default:
        {
            return std::nullopt;
        }
    }    
}


std::optional<Type> get_type(Parser &parser,std::string &type_literal)
{
    auto tok = next_token(parser);


    auto type = get_plain_type(tok,type_literal);

    if(!type)
    {
        panic("invalid plain type");
        return std::nullopt;
    }

    // check for any specifiers i.e '@'
    switch(peek(parser,0).type)
    {
        case token_type::deref:
        {
            panic("dereference not implemented");
            tok = next_token(parser);
            break;
        }

        // we have just a plain type
        default: break;
    }


    return type;
}

AstNode *declaration(Parser &parser,const Type &var_type, const std::string &type_str)
{
    // declartion
    // type symbol ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: %s:%zd\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    //    [declare:name]
    // [type]   optional([eqauls])

    auto d = new AstNode(ast_type::declaration,s.literal);
    d->nodes.push_back(new AstNode(var_type,type_str));

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
            if(e && e->nodes.size() >= 1)
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

    const auto d = new AstNode(ast_type::auto_decl,s.literal);

    d->nodes.push_back(expr(parser,next_token(parser)));

    return d;    
}

AstNode *statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        // handle builtin types
        case token_type::u8:
        case token_type::u16:
        case token_type::u32:
        case token_type::s8:
        case token_type::s16:
        case token_type::s32:
        case token_type::bool_t:
        {
            prev_token(parser);
            std::string type_literal;
            const auto var_type_opt = get_type(parser,type_literal);

            if(!var_type_opt)
            {
                type_panic(parser);
                return nullptr;
            }

            const auto var_type = var_type_opt.value();
            return declaration(parser,var_type,type_literal);
        }

    
        case token_type::decl:
        {
            return auto_decl(parser);
        }

        case token_type::ret:
        {
            auto r = new AstNode(ast_type::ret);

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

        // TODO: detect a declartion with a user defined type
        case token_type::symbol:
        {
            const auto t2 = peek(parser,0);

            switch(t2.type)
            {
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
            const auto for_block = new AstNode(ast_type::for_block);

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
                
                const u32 old = parser.tok_idx;

                std::string type_literal = "";
                const auto type_opt = get_type(parser,type_literal);

                // allow declaration for mult type statement
                if(type_opt)
                {
                    const auto type = type_opt.value();

                    for_block->nodes.push_back(declaration(parser,type,type_literal));
                    terminator = token_type::semi_colon; 
                }

                // some other statement (maybe a boolean one for a single stmt)
                else
                {
                    parser.tok_idx = old;
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
            const auto if_block = new AstNode(ast_type::if_block);

            const auto if_stmt = new AstNode(ast_type::if_t);

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

                        const auto else_if_stmt = new AstNode(ast_type::else_if_t);

                        else_if_stmt->nodes.push_back(expr_terminate(parser,token_type::left_c_brace)); prev_token(parser); 
                        else_if_stmt->nodes.push_back(block(parser));

                        if_block->nodes.push_back(else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        const auto else_stmt = new AstNode(ast_type::else_t);

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

    auto b = new AstNode(ast_type::block);

    
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

    // first check this is a valid function definiton and consume it 
    // func_dec = func return_type ident(arg...)
    // arg = type ident

    // can be void (i.e we have no return type)
    std::string return_type_literal;
    Type return_type;
    
    // void
    if(peek(parser,0).type == token_type::symbol && peek(parser,1).type == token_type::left_paren)
    {
        return_type_literal = "void";
        return_type = Type(builtin_type::void_t);
    }

    // type specified
    else
    {
        const auto return_type_opt = get_type(parser,return_type_literal);

        if(!return_type_opt)
        {
            type_panic(parser);
            return nullptr;
        }


        return_type = return_type_opt.value();
    }

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        panic(parser,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return nullptr;  
    }

    auto f = new AstNode(ast_type::function, func_name.literal);
    


    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);

    auto a = new AstNode(ast_type::function_args);

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            delete_tree(a);
            delete_tree(f);
            panic(parser,paren,"unterminated function declaration!");
            return nullptr;
        }

        // for each arg pull type, name
        std::string type_name;
        const auto type_opt = get_type(parser,type_name);

        if(!type_opt)
        {
            type_panic(parser);
            delete_tree(a);
            delete_tree(f);
            return nullptr;
        }

        const auto type = type_opt.value();

        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            delete_tree(a);
            delete_tree(f);
            panic(parser,lit_tok,"expected name for function arg");
            return nullptr;
        }
        
        // add each declartion
        auto d = new AstNode(ast_type::declaration,lit_tok.literal);
        d->nodes.push_back(new AstNode(type,type_name));

        a->nodes.push_back(d);

        // if the declaration isnt closed get the next arg
        if(peek(parser,0).type != token_type::right_paren)
        {
            consume(parser,token_type::comma);
        }
    }

    consume(parser,token_type::right_paren);

    // no args is fine

    auto b = block(parser);


    //      [func: name]
    // [type] [block]  [args]
    f->nodes.push_back(new AstNode(return_type,return_type_literal));
    f->nodes.push_back(b);
    f->nodes.push_back(a);

    return f;
}


bool parse(AstNode **root_ptr, const std::vector<Token> &tokens, const std::vector<std::string> &lines)
{
    panic(!root_ptr,"attempted to parse into null tree");

    *root_ptr = new AstNode(ast_type::root);
    const auto size = tokens.size();


    Parser parser;
    parser.tokens = &tokens;

    while(parser.tok_idx < size)
    {
        const auto &t = next_token(parser);
        // okay what is our "top level" token
        switch(t.type)
        {
            // function declartion
            case token_type::func:
            {
                (*root_ptr)->nodes.push_back(func(parser));
                break;
            }


            default:
            {
                panic(parser,t,"unexpected token %s: %s\n",tok_name(t.type),t.literal.c_str());
                delete_tree(*root_ptr); *root_ptr = nullptr;
                break;
            }
        }

        if(parser.error)
        {
            // print line number
            printf("%s\n",lines[parser.line].c_str());

            break;
        }
    }

    return parser.error;
}

void print(const AstNode *root)
{
    if(!root)
    {
        return;
    }

    if(root->type == ast_type::function)
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
        if(n)
        {
            print(n);
        }
    }
    depth -= 1;
}
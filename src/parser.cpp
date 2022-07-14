#include <interloper.h>
#include "expression.cpp"

void type_panic(Parser &parser);
BlockNode *block(Parser &parser);


const u32 AST_ALLOC_DEFAULT_SIZE = 8 * 1024;

Parser make_parser(const String& cur_file,ArenaAllocator* ast_allocator,ArenaAllocator* string_allocator, AstPointers* ast_arrays)
{
    Parser parser;
    parser.allocator = ast_allocator;
    parser.string_allocator = string_allocator;
    parser.cur_file = cur_file;
    parser.ast_arrays = ast_arrays;

    return parser;
}

void add_ast_pointer(Parser& parser, void* pointer)
{
    push_var(*parser.ast_arrays,pointer);
}

Token next_token(Parser &parser)
{
    if(parser.tok_idx >= count(parser.tokens))
    {
        // TODO: make this return the actual file end
        // for row and col
        return token_plain(token_type::eof,0,0);
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
    if(idx >= count(parser.tokens))
    {
        return token_plain(token_type::eof,0,0);
    }

    return parser.tokens[idx];
}



void consume(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        panic(parser,tok,"expected %s got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
}

bool match(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    return t == type;
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


TypeNode *parse_type(Parser &parser)
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

    String type_literal;
    
    if(type_idx == STRUCT_IDX)
    {   
        type_literal = plain_tok.literal;
    }

    else
    {
        type_literal = TYPE_NAMES[type_idx];
    }

    TypeNode* type = (TypeNode*)ast_type_decl(parser,type_literal,plain_tok);
    type->type_idx = type_idx;
    type->is_const = is_const;


    b32 quit = false;

    while(!quit)
    {
        // parse out other types, arrays, pointers
        switch(peek(parser,0).type)
        {
            // pointer decl
            case token_type::deref:
            {
                while(peek(parser,0).type == token_type::deref)
                {
                    next_token(parser);
                    type->ptr_indirection++;
                }
                break;
            }


            // array decl
            case token_type::sl_brace:
            {
                RecordNode* arr_decl = (RecordNode*)ast_record(parser,ast_type::arr_dimensions,plain_tok);

                while(peek(parser,0).type == token_type::sl_brace)
                {
                    consume(parser,token_type::sl_brace);

                    // var size
                    if(peek(parser,0).type == token_type::sr_brace)
                    {
                        push_var(arr_decl->nodes,ast_plain(parser,ast_type::arr_var_size,plain_tok));
                        consume(parser,token_type::sr_brace);
                    }

                    else 
                    {
                        // figure out this size later
                        if(peek(parser,0).type == token_type::qmark)
                        {
                            consume(parser,token_type::qmark);

                            const auto e = ast_plain(parser,ast_type::arr_deduce_size,plain_tok);
                            push_var(arr_decl->nodes,e);
                        
                            consume(parser,token_type::sr_brace);
                        }

                        else
                        {
                            push_var(arr_decl->nodes,expr_terminate(parser,token_type::sr_brace));
                        }
                    }
                }

                type->arr_decl = arr_decl;
                type->contains_ptr = type->ptr_indirection != 0;
                break;
            }

            default: quit = true; break;
        }
    }


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


    TypeNode* type = parse_type(parser);

    if(!type)
    {
        type_panic(parser);
        return nullptr;
    }

    //    [declare:name]
    // [type]   optional([eqauls])

    DeclNode* decl = (DeclNode*)ast_decl(parser,s.literal,type,s);

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
            
            decl->expr = expr(parser,next_token(parser));
            break;
        }

        default:
        {
            panic(parser,eq,"malformed declartion: %s\n",tok_name(eq.type));
            break;
        }
    }

    return (AstNode*)decl;
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

    AstNode* e = expr(parser,next_token(parser));
    AstNode* decl = (AstNode*)ast_auto_decl(parser,s.literal,e,s);

    return decl;    
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
            // return value is optional
            if(peek(parser,0).type != token_type::semi_colon)
            {
                AstNode* e = expr(parser,next_token(parser));
                return ast_unary(parser,e,ast_type::ret,t);
            }

            else
            {
                consume(parser,token_type::semi_colon);
                return ast_plain(parser,ast_type::ret,t);
            }
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

            return (AstNode*)block(parser);
        }

        // assume one cond for now
        case token_type::for_t:
        {
            ForNode* for_node = (ForNode*)ast_for(parser,t);

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
                for_node->initializer = auto_decl(parser);              
            }

            // standard decl
            else
            {
                // decl 
                if(peek(parser,1).type == token_type::colon)
                {
                    for_node->initializer = declaration(parser);
                }

                // standard stmt
                else
                {
                    for_node->initializer = expr_terminate(parser,first_term,terminator); 
                }
            }


            // for(s32 x = 5; x > 0; x -= 1) (multiple statement)

            for_node->cond = expr_terminate(parser,token_type::semi_colon); 

            // allow paren terminator followed by a '{'
            if(term_paren)
            {
                for_node->post = expr_terminate(parser,token_type::right_paren);
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
                for_node->post = expr_terminate(parser,token_type::left_c_brace);
                prev_token(parser);
            }  
            

            // for stmt parsed now compile the actual block
            for_node->block = block(parser);

            return (AstNode*)for_node;
        }

        case token_type::while_t:
        {
            AstNode* while_expr = expr_terminate(parser,token_type::left_c_brace); prev_token(parser); 
            AstNode* while_body = (AstNode*)block(parser);

            AstNode* while_stmt = ast_binary(parser,while_expr,while_body,ast_type::while_block,t);

            return while_stmt;
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            BlockNode* if_block = (BlockNode*)ast_if_block(parser,t);


            AstNode* if_expr = expr_terminate(parser,token_type::left_c_brace); prev_token(parser); 
            AstNode* if_body = (AstNode*)block(parser);

            BinNode *if_stmt = (BinNode*)ast_binary(parser,if_expr,if_body,ast_type::if_t,t);

            push_var(if_block->statements,if_stmt);
            
            bool done = false;
            
            while(!done)
            {
                if(peek(parser,0).type == token_type::else_t)
                {
                    auto else_tok = next_token(parser);

                    // we have an else if
                    if(peek(parser,0).type == token_type::if_t)
                    {
                        consume(parser,token_type::if_t);

                        AstNode* else_if_expr = expr_terminate(parser,token_type::left_c_brace); prev_token(parser); 
                        AstNode* else_if_body = (AstNode*)block(parser);

                        BinNode* else_if_stmt = (BinNode*)ast_binary(parser,else_if_expr,else_if_body,ast_type::else_if_t,else_tok);

                        push_var(if_block->statements,else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        AstNode* else_body = (AstNode*)block(parser);
                        AstNode *else_stmt = ast_unary(parser,else_body,ast_type::else_t,else_tok);

                        push_var(if_block->statements,else_stmt);

                        done = true;
                    }
                }

                // this chain is done we have another token
                else
                {
                    done = true;
                }
            }
            return (AstNode*)if_block;
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

BlockNode *block(Parser &parser)
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(parser,0);
    consume(parser,token_type::left_c_brace);

    BlockNode* b = (BlockNode*)ast_block(parser,tok);

    
    // parse out all our statements
    while(!match(parser,token_type::right_c_brace))
    {
        if(match(parser,token_type::eof))
        {
            panic(parser,tok,"unterminated block!\n");
            return nullptr;
        }

        push_var(b->statements,statement(parser));

        if(parser.error)
        {
            return b;
        }
    }
    

    consume(parser,token_type::right_c_brace);

    return b;
}


void func_decl(Interloper& itl, Parser &parser, const String& filename)
{

    // func_dec = func ident(arg...) return_type 
    // arg = ident : type,

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        panic(parser,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return;
    }

    if(contains(itl.function_table,func_name.literal))
    {
        panic(itl,"function %s has been declared twice!\n",func_name.literal.buf);
        return;
    }

    FuncNode *f = (FuncNode*)ast_func(parser,func_name.literal,filename,func_name);

    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);


    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            panic(parser,paren,"unterminated function declaration!\n");
            return;
        }

        // for each arg pull type, name
       

        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            panic(parser,lit_tok,"expected name for function arg\n");
            return;
        }
        

        consume(parser,token_type::colon);

        TypeNode* type = parse_type(parser);

        if(!type)
        {
            type_panic(parser);
            return;
        }


        // add each declartion
        DeclNode* decl = (DeclNode*)ast_decl(parser,lit_tok.literal,type,lit_tok);
        
        push_var(f->args,decl);

        // if the declaration isnt closed get the next arg
        if(peek(parser,0).type != token_type::right_paren)
        {
            consume(parser,token_type::comma);
        }
    }

    consume(parser,token_type::right_paren);


    if(!match(parser,token_type::left_c_brace))
    {
        f->return_type = parse_type(parser);

        if(!f->return_type)
        {
            type_panic(parser);
        }
    }

    // void
    else
    {
        f->return_type = (TypeNode*)ast_type_decl(parser,"void",func_name); 
        f->return_type->type_idx = u32(builtin_type::void_t);
    }

    f->block = block(parser); 


    // finally add the function def
    Function func;
    func.name = copy_string(itl.string_allocator,func_name.literal);
    func.root = f;

    add(itl.function_table,func.name,func);
}

void struct_decl(Interloper& itl,Parser& parser, const String& filename)
{
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        panic(parser,name,"expected name after struct decl got %s\n",tok_name(name.type));
        return;
    }

    if(contains(itl.struct_def,name.literal))
    {
        panic(itl,"struct %s redeclared\n",name.literal.buf);
        return;
    }

    StructNode* struct_node = (StructNode*)ast_struct(parser,name.literal,filename,name);

    consume(parser,token_type::left_c_brace);

    while(!match(parser,token_type::right_c_brace))
    {
        DeclNode* decl = (DeclNode*)declaration(parser);

        if(!decl)
        {
            panic(parser,name,"malformed struct member decl\n");
            return;
        }

        push_var(struct_node->members,decl);
    }

    consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        consume(parser,token_type::semi_colon);
    }

    // TODO: we now should check redefiniton here?
    StructDef definition = {struct_state::not_checked,struct_node,0};

    add(itl.struct_def,name.literal,definition);
}

Array<char> read_source_file(const String& filename)
{
    Array<char> file = read_file(filename);
    if(count(file))
    {
        printf("no such file: %s\n",filename.buf);
        exit(0);
    }    

    return file;
}


void add_file(HashTable<String,u32> &file_set, Array<String>& stack, const String& filename)
{
    if(!contains(file_set,filename))
    {
        add(file_set,filename,u32(0));
        push_var(stack,filename);
    }
}

String get_program_name(ArenaAllocator& allocator,const String& filename)
{
    if(!contains_ext(filename))
    {
        return cat_string(allocator,filename,".itl");
    }

    return filename;
}


void destroy_parser(Parser& parser)
{
    destroy_arr(parser.tokens);
}

bool parse_file(Interloper& itl,const String& file, const String& filename,const String& stl_path, HashTable<String,u32>& file_set, Array<String> &file_stack)
{
    // Parse out the file
    Parser parser = make_parser(filename,&itl.ast_allocator,&itl.ast_string_allocator,&itl.ast_arrays);

    if(tokenize(file,parser.string_allocator,parser.tokens))
    {
        printf("failed to tokenize file: %s\n",filename.buf);
        destroy_arr(parser.tokens);
        return true;
    }
    
    //print_tokens(parser.tokens);

    const auto size = count(parser.tokens);

    // TODO: move this to a seperate loop to make freeing up crap ez
    // TODO: put an extra string in the top level decl of the ast
    // so we know what file it came from
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
                    panic(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(t.type),t.literal.buf);
                    destroy_arr(parser.tokens);
                    return true;
                }

                const auto name_tok = next_token(parser);

                // stl file
                if(!contains_ext(name_tok.literal))
                {
                    add_file(file_set,file_stack, cat_string(itl.string_allocator,stl_path,get_program_name(itl.string_allocator,name_tok.literal)));
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
                func_decl(itl,parser,filename);
                break;
            }

            case token_type::struct_t:
            {

                struct_decl(itl,parser,filename);
                break;
            }

            default:
            {
                panic(parser,t,"unexpected top level token %s: '%s'\n",tok_name(t.type),t.literal.buf);
                destroy_arr(parser.tokens);
                return true;
            }
        }

        if(parser.error)
        {
            // print line number
            print_line(filename,parser.line);
            destroy_arr(parser.tokens);
            return true;
        }
    }

    destroy_arr(parser.tokens);
    return false;
}

bool parse(Interloper& itl, const String& initial_filename)
{
    // TODO: destruct these
    Array<String> file_stack;
    HashTable<String,u32> file_set = make_table<String,u32>();

    // add the initial file
    add_file(file_set,file_stack,get_program_name(itl.string_allocator,initial_filename));



    // TODO: this should probably be a SHELL VAR but just hard code it for now
    const String stl_path = make_static_string("stl/",strlen("stl/"));

    // import basic by default
    add_file(file_set,file_stack,cat_string(itl.string_allocator,stl_path,"basic.itl"));


    b32 error = false;

    while(count(file_stack))
    {
        // get the next filename to parse
        const String filename = pop(file_stack);

        Array<char> file = read_file(filename);

        if(!count(file))
        {
            printf("file %s does not exist\n",filename.buf);
            error = true;
            break;
        }

        error = parse_file(itl,make_string(file),filename,stl_path,file_set,file_stack);

        destroy_arr(file);

        if(error)
        {
            break;
        }
    }

    destroy_arr(file_stack);
    destroy_table(file_set);

    return error;
}


void print_depth(int depth)
{
    for(int i = 0; i < depth; i++)
    { 
        printf(" -");
    }
    printf(" %d ",depth);    
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
    
    depth += 1;

    print_depth(depth);

    switch(root->fmt)
    {
        case ast_fmt::plain:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);
            break;
        }

        case ast_fmt::binary:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);

            BinNode* bin_node = (BinNode*)root;

            print(bin_node->left);
            print(bin_node->right);

            break;
        }

        case ast_fmt::unary:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);

            UnaryNode* unary_node = (UnaryNode*)root;

            print(unary_node->next);
            break;
        }

        case ast_fmt::literal:
        {
            LiteralNode* literal_node = (LiteralNode*)root;

            printf(" %s : %s\n",AST_NAMES[static_cast<size_t>(root->type)],literal_node->literal.buf);
            break;
        }

        case ast_fmt::value:
        {
            ValueNode* value_node = (ValueNode*)root;

            printf("value: %s%d\n",value_node->value.sign? "-" : "",value_node->value.v);
            break;
        }


        case ast_fmt::function:
        {
            FuncNode* func_node = (FuncNode*)root;

            printf("function %s:%s\n",func_node->filename.buf,func_node->name.buf);

            for(u32 a = 0; a < count(func_node->args); a++)
            {
                print((AstNode*)func_node->args[a]);
            }            

            print((AstNode*)func_node->block);

            print((AstNode*)func_node->return_type);
            break;
        }

        case ast_fmt::struct_t:
        {
            StructNode* struct_node = (StructNode*) root;

            printf("struct %s:%s\n",struct_node->filename.buf,struct_node->name.buf);

            for(u32 m = 0; m < count(struct_node->members); m++)
            {
                print((AstNode*)struct_node->members[m]);
            }                        

            break;
        }

        case ast_fmt::char_t:
        {
            CharNode* char_node = (CharNode*)root;
            
            printf("char: '%c' : %d\n",char_node->character,char_node->character);

            break;
        }

        case ast_fmt::type:
        {
            TypeNode* type_decl = (TypeNode*)root;
            printf("type: %s ", type_decl->is_const? "const" : "");

            const u32 dimensions = type_decl->arr_decl? count(type_decl->arr_decl->nodes) : 0;


            if(!type_decl->contains_ptr)
            {
                print_str("[]",dimensions);
                print_str("@",type_decl->ptr_indirection);
            }

            // could be array of pointers
            else
            {
                print_str("@",type_decl->ptr_indirection);
                print_str("[]",dimensions);
            } 


            printf("%s\n",type_decl->name.buf);
            
            if(type_decl->arr_decl)
            {
                print((AstNode*)type_decl->arr_decl);
            }
            break;
        }

        case ast_fmt::declaration:
        {
            DeclNode* decl_node = (DeclNode*)root;

            printf("decl : %s\n",decl_node->name.buf);

            print((AstNode*)decl_node->type);
            print(decl_node->expr);
            break;
        }

        case ast_fmt::auto_decl:
        {
            assert(false);
            break;
        }

        case ast_fmt::block:
        {
            printf("block\n");
            BlockNode* block_node = (BlockNode*)root;

            for(u32 s = 0; s < count(block_node->statements); s++)
            {
                print((AstNode*)block_node->statements[s]);
            }                        

            break;
        }

        case ast_fmt::function_call:
        {
            FuncCallNode* func_call = (FuncCallNode*)root;

            printf("function call : %s\n",func_call->name.buf);

            for(u32 a = 0; a < count(func_call->args); a++)
            {
                print(func_call->args[a]);
            }                                    

            break;
        }

        case ast_fmt::for_block:
        {
            printf("for\n");

            ForNode* for_node = (ForNode*)root;

            print(for_node->initializer);
            print(for_node->cond);
            print(for_node->post);

            print((AstNode*)for_node->block);

            break;
        }

        case ast_fmt::record:
        {
            printf("%s\n",AST_NAMES[static_cast<size_t>(root->type)]);


            RecordNode* record_node = (RecordNode*)root;

            for(u32 n = 0; n < count(record_node->nodes); n++)
            {
                print(record_node->nodes[n]);
            }                 

            break;
        }

        case ast_fmt::index:
        {
            printf("index\n");

            IndexNode* index_node = (IndexNode*)root;

            for(u32 i = 0; i < count(index_node->indexes); i++)
            {
                print(index_node->indexes[i]);
            }

            break;
        }
    }

    depth -= 1;
}
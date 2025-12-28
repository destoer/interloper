#include <interloper.h>
#include <unistd.h>

Option<parse_error> block_ast(Parser &parser, AstBlock* block);
Result<AstBlock*,parse_error> block_ast_unpinned(Parser &parser);
Option<ParserResult> try_parse_slice(Parser& parser, const Token& t);
Result<FuncNode*,parse_error> parse_func_sig(Parser& parser, const String& func_name,const Token& token);
ParserResult statement(Parser &parser);


const u32 AST_ALLOC_DEFAULT_SIZE = 8 * 1024;

Parser make_parser(const String& cur_file,NameSpace* root, ArenaAllocator* namespace_allocator,
    ArenaAllocator *global_string_allocator,ArenaAllocator* ast_allocator,ArenaAllocator* string_allocator, AstPointers* ast_arrays)
{
    Parser parser;
    parser.ast_allocator = ast_allocator;
    parser.string_allocator = string_allocator;
    parser.global_string_allocator = global_string_allocator;
    parser.namespace_allocator = namespace_allocator;

    // NOTE: this relies on get_program_name to allocate the string correctly
    parser.cur_file = cur_file;
    parser.cur_path = extract_path(parser.cur_file);
    parser.ast_arrays = ast_arrays;
    parser.global_namespace = root;
    parser.cur_namespace = parser.global_namespace;

    return parser;
}

void add_ast_pointer(Parser& parser, void* pointer)
{
    push_raw_var(*parser.ast_arrays,pointer);
}

Token next_token(Parser &parser)
{
    if(parser.tok_idx >= count(parser.tokens))
    {
        // TODO: make this return the actual file end
        // for row and col
        return token_plain(token_type::eof,0);
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
        return token_plain(token_type::eof,0);
    }

    return parser.tokens[idx];
}


Option<parse_error> consume(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        return parser_error(parser,parse_error::unexpected_token,tok,"expected '%s' got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
    return option::none;
}

bool match(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    return t == type;
}

bool is_builtin_type_tok(const Token &tok)
{
   return tok.type >= token_type::u8 && tok.type <= token_type::f64_t; 
}

builtin_type builtin_type_from_tok(const Token& tok)
{
    // compute builtin type idx 
    return builtin_type(s32(tok.type) - s32(token_type::u8));
}

#include "parser/expression.cpp"
#include "parser/variable.cpp"
#include "parser/type.cpp"
#include "parser/control_flow.cpp"
#include "parser/function.cpp"


ParserResult const_assert(Parser& parser,const Token& t)
{
    const auto left_paren_err = consume(parser,token_type::left_paren);
    if(left_paren_err)
    {
        return *left_paren_err;
    }

    const auto expr = expr_terminate(parser,"const_assert",token_type::right_paren);
    const auto right_paren_err = consume(parser,token_type::semi_colon);
    if(right_paren_err)
    {
        return *right_paren_err;
    }

    return ast_const_assert(parser,expr,t);       
}


ParserResult parse_ret(Parser& parser, const Token& t)
{
    // return value is optional
    if(!match(parser,token_type::semi_colon))
    {
        if(peek(parser,1).type == token_type::left_c_brace)
        {
            auto initializer_res = parse_struct_initializer(parser);

            if(!initializer_res)
            {
                return initializer_res;
            }

            auto initializer = (StructInitializerNode*)initializer_res.value();
            initializer->is_return = true;

            return (AstNode*)initializer;
        }

        RetNode* ret_node = ast_ret(parser,t);
        b32 done = false;

        // can be more than one expr (comma separated)
        while(!done)
        {
            auto list_res = expr_list(parser,"return",token_type::semi_colon,&done);
            if(!list_res)
            {
                return list_res.error();
            }

            push_var(ret_node->expr,*list_res);
        }

        return (AstNode*)ret_node;
    }

    else
    {
        const auto term_err = consume(parser,token_type::semi_colon);
        if(term_err)
        {
            return *term_err;
        }
        return (AstNode*)ast_ret(parser,t);
    }
}

ParserResult statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        case token_type::ret:
        {
            return parse_ret(parser,t);
        }

        case token_type::const_assert:
        {
            return const_assert(parser,t); 
        }            


        case token_type::deref:
        {
            prev_token(parser);
            return statement_terminate(parser,"pointer deference");
        }


        // tuple assign
        case token_type::sl_brace:
        {
            return tuple_assign(parser, t);
        }

        case token_type::constant_t:
        {
            return declaration(parser,token_type::semi_colon,true);
        }

        case token_type::symbol:
        {
            const auto t2 = peek(parser,0);

            switch(t2.type)
            {
                // declaration with specified type
                case token_type::colon:
                {
                    prev_token(parser);
                    return declaration(parser,token_type::semi_colon);    
                }

                case token_type::decl:
                {
                    prev_token(parser);
                    return auto_decl(parser);
                }

                // assignment expr
                case token_type::plus_eq:
                case token_type::minus_eq:
                case token_type::divide_eq:
                case token_type::times_eq:
                case token_type::bitwise_or_eq:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"assignment");
                }

                case token_type::equal:
                {
                    const auto sym_tok = t;

                    // Struct assign
                    if(peek(parser,1).type == token_type::symbol && peek(parser,2).type == token_type::left_c_brace)
                    {
                        (void)consume(parser,token_type::equal);
                        AstNode* left = ast_symbol(parser,nullptr,sym_tok.literal,sym_tok);

                        return ast_equal(parser,left,parse_struct_initializer(parser),sym_tok);  
                    }

                    prev_token(parser);
                    return statement_terminate(parser,"assignment");
                }


                // check for brackets
                // array indexes etc here 

                // function call
                case token_type::left_paren:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"function call");
                }

                // array access
                case token_type::sl_brace:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"array access");
                }

                // expr for member access?
                case token_type::dot:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"struct access");
                }

                case token_type::scope:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"scope stmt");
                }


                default:
                {
                    return parser_error(parser,parse_error::unexpected_token,t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                }
            }
            break;
        }

        case token_type::left_c_brace:
        {
            // block expects to see the left c brace
            prev_token(parser);

            BlockNode* block_node = ast_block(parser,t);
            auto block_err = block_ast(parser,&block_node->block);

            if(block_err)
            {
                return *block_err;
            }

            return (AstNode*)block_node;
        }

        // assume one cond for now
        case token_type::for_t:
        {
            return parse_for(parser,t);
        }

        case token_type::while_t:
        {
            return parse_while(parser,t);
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            return parse_if(parser,t);
        }

        case token_type::switch_t:
        {
            return parse_switch(parser,t);
        }

        // dont care
        case token_type::semi_colon:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"Lone semi colon");
        }

        case token_type::ignore:
        {
            prev_token(parser);
            return statement_terminate(parser,"Ignored assign");
        }

        default:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"statement: unexpected token '%s' : %d\n",tok_name(t.type),u32(t.type));
        }
    }

    assert(false);
    return parse_error::malformed_stmt;
}



StringBuffer read_source_file(const String& filename)
{
    auto [file,err] = read_str_buf(filename);
    if(err)
    {
        printf("no such file: %s\n",filename.buf);
        exit(0);
    }    

    return file;
}


struct FileQueue
{
    Set<String> set;
    Array<String> stack;
};

void add_file(FileQueue& queue, const String& filename)
{
    if(!contains(queue.set,filename))
    {
        add(queue.set,filename);
        push_var(queue.stack,filename);
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

Result<u32,parse_error> parse_attr(Parser& parser, const Token& tok)
{
    u32 flags = 0;

    const auto left_paren_err = consume(parser,token_type::left_paren);
    if(left_paren_err)
    {
        return *left_paren_err;
    }

    const auto attr = next_token(parser);

    if(attr.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,tok,"Expected name for attr got %s\n",tok_name(attr.type));
    }

    const auto attr_name = attr.literal;

    // TODO: change this to a pregenned hashtable if it starts getting big
    if(attr_name == "no_reorder")
    {
        flags |= ATTR_NO_REORDER;
    }

    else if(attr_name == "flag")
    {
        flags |= ATTR_FLAG;
    }

    else if(attr_name == "use_result")
    {
        flags |= ATTR_USE_RESULT;
    }

    else
    {
        return parser_error(parser,parse_error::malformed_stmt,tok,"Unknown attr %s\n",attr_name.buf);
    }

    const auto right_paren_err = consume(parser,token_type::right_paren);
    if(right_paren_err)
    {
        return *right_paren_err;
    }

    return flags;
}

Option<parse_error> parse_directive(Interloper& itl,Parser& parser)
{
    const auto next = next_token(parser);

    if(next.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::missing_expr,next,"Expected name for directive got %s\n",tok_name(next.type));
    }

    // TODO: move this lookup to a hashtable if it starts getting large
    const auto name = next.literal;

    if(name == "attr")
    {
        const auto flags_res = parse_attr(parser,next);

        if(!flags_res)
        {
            return flags_res.error();
        }

        const u32 flags = *flags_res;

        const auto stmt = peek(parser,0);

        // look at which declaration is next?
        switch(stmt.type)
        {
            case token_type::struct_t:
            {
                (void)consume(parser,token_type::struct_t);
                
                const auto struct_decl_err = struct_decl(itl,parser,flags);
                if(struct_decl_err)
                {
                    return struct_decl_err;
                }
                break;
            }

            case token_type::enum_t:
            {
                (void)consume(parser,token_type::enum_t);

                const auto enum_decl_err = enum_decl(itl,parser,flags);
                if(enum_decl_err)
                {
                    return enum_decl_err;
                } 
                break; 
            }

            case token_type::func:
            {
                (void)consume(parser,token_type::func);

                const auto func_err = func_decl(itl,parser,flags);
                if(func_err)
                {
                    return func_err;
                }
                break;        
            }


            default:
            {
                return parser_error(parser,parse_error::malformed_stmt,stmt,"Attribute is not legal on stmt: %s\n",tok_name(stmt.type));
            }
        }
    }

    else
    {
        return parser_error(parser,parse_error::unexpected_token,next,"Unknown directive %s\n",name.buf);
    }

    return option::none;
}

Result<Array<String>,parse_error> split_namespace_internal(Parser& parser, const Token& start, bool full_namespace)
{
    Array<String> name_space;

    while(!match(parser,token_type::eof))
    {
        const auto name = next_token(parser);

        if(name.type != token_type::symbol)
        {
            const auto res = parser_error(parser,parse_error::unexpected_token,name,"Expected name for namespace got: %s\n",tok_name(name.type));
            destroy_arr(name_space);
            return res;
        }   

        push_var(name_space,name.literal);

        if(match(parser,token_type::scope))
        {
            const auto scope_err = consume(parser,token_type::scope);
            if(scope_err)
            {
                destroy_arr(name_space);
                return *scope_err;
            }

            // Last token after :: is not to be treated as part of the namespace
            if(peek(parser,1).type != token_type::scope && !full_namespace)
            {
                goto done;
            }
        }

        else
        {
            goto done;
        }
    }

done:
    if(count(name_space) == 0)
    {
        destroy_arr(name_space);
        return parser_error(parser,parse_error::missing_expr,start,"Namespace is empty");
    }

    return name_space;
}

Result<Array<String>,parse_error> split_namespace(Parser& parser, const Token& start)
{
    return split_namespace_internal(parser,start,false);
}

Result<Array<String>,parse_error>  split_full_namespace(Parser& parser, const Token& start)
{
    return split_namespace_internal(parser,start,true);
}



Option<parse_error> parse_top_level_token(Interloper& itl, Parser& parser, FileQueue& queue)
{
    const auto &t = next_token(parser);

    switch(t.type)
    {
        case token_type::import:
        {
            // stl path: import <name>
            if(match(parser,token_type::logical_lt))
            {
                const auto lt_err = consume(parser,token_type::logical_lt);
                if(lt_err)
                {
                    return lt_err;
                }

                if(!match(parser,token_type::symbol))
                {
                    const auto err = next_token(parser);
                    return parser_error(parser,parse_error::missing_expr,next_token(parser),"expected string for import got %s : %s\n",
                        tok_name(err.type),err.literal.buf);
                }

                const auto name_tok = next_token(parser);

                const auto gt_err = consume(parser,token_type::logical_gt);
                if(gt_err)
                {
                    return gt_err;
                }

                const auto full_path = cat_string(itl.string_allocator,itl.stl_path,get_program_name(itl.string_allocator,name_tok.literal)); 

                add_file(queue, full_path);
            }

            // relative path: import "boop"
            else if(match(parser,token_type::string))
            {
                const auto name_tok = next_token(parser);

                const auto full_path = cat_string(itl.string_allocator,parser.cur_path,get_program_name(itl.string_allocator,name_tok.literal));

                add_file(queue,full_path);
            }

            // unk
            else
            {
                return parser_error(parser,parse_error::missing_expr,next_token(parser),"expected string for import got %s : %s\n",
                    tok_name(t.type),t.literal.buf);
            }
            break;
        }

        // function declartion
        case token_type::func:
        {
            const auto func_err = func_decl(itl,parser,0);
            if(func_err)
            {
                return func_err;
            }
            break;
        }

        case token_type::struct_t:
        {
            const auto struct_err = struct_decl(itl,parser);
            if(struct_err)
            {
                return struct_err;
            }
            break;
        }

        case token_type::enum_t:
        {
            const auto enum_err = enum_decl(itl,parser,0);
            if(enum_err)
            {
                return enum_err;
            }
            break;
        }

        case token_type::type_alias:
        {
            const auto type_err = type_alias(itl,parser);
            if(type_err)
            {
                return type_err;
            }
            break;
        }

        // global constant
        case token_type::constant_t:
        {
            auto decl_res = declaration(parser,token_type::semi_colon,true);
            if(!decl_res)
            {
                return decl_res.error();
            }

            DeclNode* decl = (DeclNode*)decl_res.value();

            GlobalDeclNode* const_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_namespace,t);

            push_var(itl.constant_decl,const_decl);
            break; 
        }

        // global mut
        case token_type::global_t:
        {
            auto decl_res = declaration(parser,token_type::semi_colon);
            if(!decl_res)
            {
                return decl_res.error();
            }

            DeclNode* decl = (DeclNode*)decl_res.value();

            GlobalDeclNode* global_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_namespace,t);

            push_var(itl.global_decl,global_decl);
            break; 
        }

        case token_type::namespace_t:
        {
            auto name_space_res = split_full_namespace(parser,t);
            if(!name_space_res)
            {
                return name_space_res.error();
            }

            auto name_space = *name_space_res;

            parser.cur_namespace = scan_namespace(parser,name_space);

            destroy_arr(name_space);

            if(match(parser,token_type::semi_colon))
            {
                (void)consume(parser,token_type::semi_colon);
            }
            break;
        }


        default:
        {
            if(t.type == token_type::symbol)
            {
                return parser_error(parser,parse_error::unexpected_token,t,"unexpected top level symbol '%s'\n",t.literal.buf);
            }

            else
            {
                return parser_error(parser,parse_error::unexpected_token,t,"unexpected top level token '%s' : (%d)\n",tok_name(t.type),u32(t.type));
            }

            break;
        }
    }

    return option::none;
}

Option<parse_error> parse_file(Interloper& itl,const String& file, const String& filename,FileQueue& queue)
{
    // Parse out the file
    Parser parser = make_parser(filename,itl.global_namespace,&itl.namespace_allocator,&itl.string_allocator,&itl.ast_allocator,&itl.ast_string_allocator,&itl.ast_arrays);

    if(tokenize(file,filename,parser.string_allocator,parser.tokens))
    {
        destroy_parser(parser);
        itl.first_error_code = itl_error::lexer_error;
        return parse_error::lexer_error;
    }
    
    if(itl.print_tokens)
    {
        printf("tokens for file: %s\n",filename.buf);
        print_tokens(parser.tokens);
    }
    
    const auto size = count(parser.tokens);

    while(parser.tok_idx < size)
    {
        // check for a directive
        if(match(parser,token_type::hash))
        {
            const auto hash_err = consume(parser,token_type::hash);
            if(hash_err)
            {
                destroy_parser(parser);
                return hash_err;
            }

            const auto directive_err = parse_directive(itl,parser);
            if(directive_err)
            {
                destroy_parser(parser);
                return directive_err;
            }
        }

        // plain decl
        else
        {
            const auto parse_err = parse_top_level_token(itl,parser,queue);
            if(parse_err)
            {
                destroy_parser(parser);
                return parse_err;
            }
        }
    }

    destroy_parser(parser);
    return option::none;
}

Option<parse_error> parse(Interloper& itl, const String& initial_filename)
{
    const char *itl_path = nullptr;

    if(file_exists("interloper"))
    {
        itl_path = ".";
    }

    else
    {
        itl_path = getenv("INTERLOPER_INSTALL_DIR");

        if(!itl_path)
        {
            fprintf(stderr,"Could not find install dir env var INTERLOPER_INSTALL_DIR\n");
            return parse_error::itl_error;
        }
    }

    itl.stl_path = cat_string(itl.string_allocator,itl_path,"/stl/");

    FileQueue queue;
    queue.set = make_set<String>();

    
    // add the initial file
    add_file(queue,get_program_name(itl.string_allocator,initial_filename));


    // import basic by default
    add_file(queue,cat_string(itl.string_allocator,itl.stl_path,"basic.itl"));

    add_file(queue,cat_string(itl.string_allocator,itl.stl_path,"internal.itl"));

    Option<parse_error> err = option::none;

    while(count(queue.stack))
    {
        // get the next filename to parse
        const String filename = pop(queue.stack);

        auto [file,file_err] = read_str_buf(filename);

        if(file_err)
        {
            printf("file %s does not exist\n",filename.buf);
            err = parse_error::itl_error;
            break;
        }

        err = parse_file(itl,make_string(file),filename,queue);

        destroy_arr(file);

        if(err)
        {
            break;
        }
    }

    destroy_arr(queue.stack);
    destroy_set(queue.set);

    return err;
}


void print_depth(int depth)
{
    for(int i = 0; i < depth; i++)
    { 
        printf(" -");
    }
    printf(" %d ",depth);    
}

void print_internal(Interloper& itl, const AstNode *root, int depth);

template<typename T>
void print_bin_oper(Interloper& itl, const ExprBinOperNode<T>* bin,const char* NAMES[], int depth)
{
    print_ast(itl,"%s %t",NAMES[u32(bin->oper)],bin->node.expr_type);

    print_internal(itl,bin->left,depth + 1);
    print_internal(itl,bin->right,depth + 1);
}

template<ast_type type>
void print_unary(Interloper& itl, UnaryNode<type>* unary, const char* name, int depth)
{
    print_ast(itl,"%s %t",name,unary->node.expr_type);
    print_internal(itl,unary->expr,depth + 1);
}

void print_block(Interloper& itl,const AstBlock* block, int depth)
{
    for(AstNode* node : block->statement)
    {
        print_internal(itl,node, depth + 1);
    }
}

void print_if_stmt(Interloper& itl,const IfStmt& stmt, const char* name, int depth)
{
    print_depth(depth + 1);
    printf("%s\n",name);
    print_internal(itl,stmt.expr, depth + 2);
    print_block(itl,stmt.block, depth + 2);
}


void print_itl(Interloper& itl, const String& fmt, va_list args)
{
    // %S  String
    // %s  string
    // %x  hex
    // %l  long hex 
    // %d  int print
    // %D  depth print
    // %t  Type
    // %n  Namespace
    // %f  float

    for(size_t i = 0; i < fmt.size; i++)
    {
        const char token = fmt[i];

        if(token != '%')
        {
            putchar(token);
            continue;
        }

        assert(i != fmt.size);

        const char specifier = fmt[++i];
        switch(specifier)
        {
            case 's':
            {
                const char* str = va_arg(args,char*);
                printf("%s",str);
                break;
            }

            case 'S':
            {
                const String str = va_arg(args,String);
                printf("%s",str.buf);

                break;
            }

            case 'x':
            {
                const u32 value = va_arg(args,u32);
                printf("0x%x",value);
                break;
            }

            case 'X':
            {
                const u64 value = va_arg(args,u64);
                printf("0x%lx",value);
                break;
            }

            case 'd':
            {
                const u32 value = va_arg(args,u32);
                printf("%d",value);
                break;
            }

            case 'l':
            {
                const u64 value = va_arg(args,u64);
                printf("%ld",value);
                break;
            }

            case 'D':
            {
                const u32 depth = va_arg(args,u32);
                print_depth(depth);
                break;
            }


            case 't':
            {
                const Type* type = va_arg(args,Type*);
                if(type)
                {
                    printf("(%s)",type_name(itl,type).buf);
                }
                break;
            }

            case 'n':
            {
                const NameSpace* name_space = va_arg(args,NameSpace*);
                if(name_space)
                {
                    printf("%s::",name_space->full_name.buf);
                }

                break;
            }

            case 'f':
            {
                const f64 value = va_arg(args,f64);
                printf("%f",value);
                break;
            }

            default: assert(false);
        }
    }
}

void print_ast(Interloper& itl, const String& fmt, ...)
{
    va_list args;
    va_start(args,fmt);

    print_itl(itl,fmt,args);

    putchar('\n');

    va_end(args);
}

String named_symbol_name(Interloper& itl, const AstNode* node, const NamedSymbol& named_sym)
{
    if(node->expr_type)
    {
        if(named_sym.slot.handle == INVALID_HANDLE)
        {
            return "";
        }

        Symbol& sym = sym_from_slot(itl.symbol_table,named_sym.slot);
        return sym.name;
    }

    return named_sym.name;
}

void print_internal(Interloper& itl,const AstNode *root, int depth)
{
    if(!root)
    {
        print_depth(depth + 1);
        puts("EMPTY");
        return;
    }


    if(root->type == ast_type::function || root->type == ast_type::struct_t || root->type == ast_type::enum_t)
    {
        printf("\n\n\n");
    }

    print_depth(depth);

    switch(root->type)
    {
        case ast_type::assign:
        {
            printf("%s\n",AST_INFO[u32(root->type)].name);

            auto equal = (AssignNode*)root;
            print_internal(itl,equal->left,depth + 1);
            print_internal(itl,equal->right,depth + 1);
            break;
        }

        case ast_type::arith_bin:
        {
            print_bin_oper(itl,(ArithBinNode*)root,ARITH_BIN_NAMES,depth);
            break;
        }

        case ast_type::shift:
        {
            print_bin_oper(itl,(ShiftNode*)root,SHIFT_NAMES,depth);
            break;    
        }


        case ast_type::comparison:
        {
            print_bin_oper(itl,(CmpNode*)root,COMPARISON_NAMES,depth);
            break;
        }

        case ast_type::boolean_logic:
        {
            print_bin_oper(itl,(BooleanLogicNode*)root,BOOLEAN_LOGIC_NAMES,depth);
            break;
        }

        case ast_type::symbol:
        {
            SymbolNode* sym_node = (SymbolNode*)root;
            String name = "";
            switch(sym_node->type)
            {
                case sym_node_type::name:
                {
                    name = sym_node->name;
                    break;
                }

                case sym_node_type::sym_slot:
                {
                    auto& sym = sym_from_slot(itl.symbol_table,sym_node->sym_slot);
                    name = sym.name;
                    break;
                }

                case sym_node_type::func_ptr:
                {
                    Function* func = sym_node->func;
                    name = func->name;
                    break;
                }
            }

            print_ast(itl,"Symbol: %n%S %t",sym_node->name_space,name,sym_node->node.expr_type);
            break;
        }

        case ast_type::arith_unary:
        {
            ArithUnaryNode* unary = (ArithUnaryNode*)root;

            print_ast(itl,"%s %t",ARITH_UNARY_NAMES[u32(unary->oper)],unary->node.expr_type);
            print_internal(itl,unary->expr,depth + 1);
            break;
        }

        case ast_type::type_operator:
        {
            TypeOperatorNode* type = (TypeOperatorNode*)root;

            printf("Type operator %s\n",TYPE_OPER_NAMES[u32(type->oper)]);
            print_internal(itl,(AstNode*)type->type,depth + 1);
            break;
        }

        case ast_type::cast:
        {
            CastNode* cast = (CastNode*)root;
            printf("Cast\n");
            print_internal(itl,(AstNode*)cast->type, depth + 1);
            print_internal(itl,cast->expr, depth + 1);

            break;
        }

        case ast_type::builtin_access:
        {
            BuiltinAccessNode* access = (BuiltinAccessNode*)root;
            printf("Builtin access %s.%s\n",builtin_type_name(access->type),access->field.buf);
            break;
        }


        case ast_type::designated_initializer_list:
        {
            DesignatedListNode* list = (DesignatedListNode*)root;

            for(const auto& initializer: list->initializer)
            {
                print_depth(depth + 1);
                print_internal(itl,initializer.expr, depth + 2);
            }

            break;
        }

        case ast_type::struct_initializer:
        {
            StructInitializerNode* initializer = (StructInitializerNode*)root;
            print_ast(itl,"Struct initializer %s %n%S",initializer->is_return? "return" : "",initializer->name_space,initializer->struct_name);
            print_internal(itl,initializer->initializer, depth + 1);
            break;
        }

        case ast_type::sizeof_t:
        {
            print_unary(itl,(SizeOfNode*)root,"sizeof",depth);
            break;
        }

        case ast_type::no_init:
        {
            printf("no_init\n");
            break;
        }

        case ast_type::ignore:
        {
            printf("ignore\n");
            break;
        }

        case ast_type::value:
        {
            ValueNode* value = (ValueNode*)root;
            print_ast(itl,"Value %X (%s) %t",value->value, builtin_type_name(value->type),value->node.expr_type);
            break;
        }

        case ast_type::float_t:
        {
            FloatNode* float_node = (FloatNode*)root;
            printf("Float %f\n",float_node->value);
            break;
        }

        case ast_type::null_t:
        {
            printf("Null\n");
            break;
        }

        case ast_type::deref:
        {
            print_unary(itl,(DerefNode*)root,"deref",depth);
            break;
        }

        case ast_type::addrof:
        {
            print_unary(itl,(AddrOfNode*)root,"addrof",depth);
            break;
        }

        case ast_type::string:
        {
            StringNode* string = (StringNode*)root;
            printf("String literal %s\n",string->string.buf);
            break;
        }

        case ast_type::initializer_list:
        {
            InitializerListNode* list = (InitializerListNode*)root;

            printf("Initializer list\n");
            for(AstNode* init : list->list)
            {
                print_internal(itl,init, depth + 1);
            }
        
            break;
        }

        case ast_type::block:
        {
            BlockNode* block = (BlockNode*)root;
            printf("Block\n");
            print_block(itl,&block->block, depth);
            break;
        }

        case ast_type::type:
        {
            TypeNode* type = (TypeNode*)root;

            print_ast(itl,"%ntype %s %S %t",type->name_space,type->is_const? "const" : "",type->name,type->node.expr_type);

            for(const auto& compound : type->compound)
            {
                print_ast(itl,"%DCompound: %s",depth + 1, COMPOUND_TYPE_NAMES[u32(compound.type)]);

                if(compound.type == compound_type::arr_fixed_size)
                {
                    print_internal(itl,compound.array_size, depth + 2);
                }
            }

            if(type->func_type)
            {
                print_internal(itl,(AstNode*)type->func_type,depth + 1);
            }
            break;
        }

        case ast_type::type_alias:
        {
            AliasNode* alias = (AliasNode*)root;
            printf("Type alias %s\n",alias->name.buf);
            print_internal(itl,(AstNode*)alias->type, depth + 1);
            break;
        }

        case ast_type::struct_t:
        {
            StructNode* struct_node = (StructNode*)root;
            printf("Struct %s %x\n",struct_node->name.buf,struct_node->attr_flags);

            for(DeclNode* member : struct_node->members)
            {
                print_internal(itl,(AstNode*)member, depth + 1);
            }

            if(struct_node->forced_first)
            {
                print_internal(itl,(AstNode*)struct_node->forced_first, depth + 1);
            }

            break;
        }

        case ast_type::enum_t:
        {
            EnumNode* enum_node = (EnumNode*)root;
            printf("Enum %s %x\n",enum_node->name.buf,enum_node->attr_flags);

            for(const auto& member : enum_node->member)
            {
                print_depth(depth + 1);
                printf("Member %s\n",member.name.buf);
                if(member.initializer)
                {
                    print_internal(itl,member.initializer, depth + 2);
                }
            }

            if(enum_node->type)
            {
                print_internal(itl,(AstNode*)enum_node->type, depth + 1);
            }

            break;
        }

        case ast_type::decl:
        {
            DeclNode* decl = (DeclNode*)root;
            const auto name = named_symbol_name(itl,root,decl->sym);

            print_ast(itl,"%sDecl %S",decl->is_const? "const ": "",name);

            print_internal(itl,(AstNode*)decl->type, depth + 1);

            if(decl->expr)
            {
                print_internal(itl,decl->expr, depth + 1);
            }
            break;
        }

        case ast_type::global_decl:
        {
            GlobalDeclNode* global_decl = (GlobalDeclNode*)root;

            if(global_decl->name_space)
            {
                printf("Global decl %s\n",global_decl->name_space->full_name.buf);
            }

            else
            {
                printf("Global decl\n");
            }

            print_internal(itl,(AstNode*)global_decl->decl, depth + 1);
            break;
        }

        case ast_type::auto_decl:
        {
            AutoDeclNode* auto_decl = (AutoDeclNode*)root;
            const auto name = named_symbol_name(itl,root,auto_decl->sym);

            print_ast(itl,"Auto decl %S",name);
            
            print_internal(itl,auto_decl->expr, depth + 1);
            break;
        }

        case ast_type::function_call:
        {
            FuncCallNode* func_call = (FuncCallNode*)root;
            printf("Function call\n");
            print_internal(itl,func_call->expr, depth + 1);
            
            for(AstNode* arg : func_call->args)
            {
                print_internal(itl,arg, depth + 2);
            }

            break;
        }

        case ast_type::tuple_assign:
        {
            TupleAssignNode* tuple = (TupleAssignNode*)root;
            printf("Tuple assign %s\n",tuple->auto_decl? "auto" : "");
            print_internal(itl,(AstNode*)tuple->func_call, depth + 1);
            
            for(AstNode* sym : tuple->symbols)
            {
                print_internal(itl,sym, depth + 1);
            }

            break;
        }

        case ast_type::struct_access:
        {
            StructAccessNode* struct_access = (StructAccessNode*)root;
            print_ast(itl,"Struct access %t",struct_access->node.expr_type);

            print_internal(itl,struct_access->expr, depth + 1);

            for(AccessMember member : struct_access->members)
            {
                if(member.type >= member_access_type::slice_t)
                {
                    print_internal(itl,member.expr, depth + 2);
                }

                else
                {
                    print_ast(itl,"%D Access member: %S %t",depth + 2,member.name,member.expr_type);
                }
            }

            break;
        }

        case ast_type::index:
        {
            IndexNode* index_node = (IndexNode*)root;

            printf("Index %s\n",index_node->name.buf);
            
            for(AstNode* index : index_node->indexes)
            {
                print_internal(itl,index, depth + 1);
            }

            break;
        }

        case ast_type::slice:
        {
            SliceNode* slice = (SliceNode*)root;

            const auto name = named_symbol_name(itl,root,slice->sym);
            print_ast(itl,"Slice %S",name);

            if(slice->lower)
            {
                print_internal(itl,slice->lower, depth + 1);
            }

            if(slice->upper)
            {
                print_internal(itl,slice->upper, depth + 1);
            }

            break;
        }

        case ast_type::for_iter:
        {
            printf("For iter\n");
            ForIterNode* for_iter = (ForIterNode*)root;

            print_internal(itl,for_iter->initializer, depth + 1);
            print_internal(itl,for_iter->cond, depth + 1);
            print_internal(itl,for_iter->post, depth + 1);

            print_block(itl,&for_iter->block, depth + 2);
            break;
        }

        case ast_type::for_range:
        {
            ForRangeNode* for_range = (ForRangeNode*)root;
            const auto name_one = named_symbol_name(itl,root,for_range->sym_one);
            const auto name_two = named_symbol_name(itl,root,for_range->sym_two);

            print_ast(itl,"For [%s%S,  %S]",for_range->flags & RANGE_FOR_TAKE_POINTER? "@" : "",name_one,name_two);
            print_internal(itl,for_range->cond, depth + 1);
            print_block(itl,&for_range->block, depth + 2);
            break;
        }

        case ast_type::switch_t:
        {
            SwitchNode* switch_node = (SwitchNode*)root;
            printf("Switch\n");

            print_internal(itl,switch_node->expr, depth + 1);

            for(const auto& switch_case : switch_node->statements)
            {
                print_internal(itl,switch_case.statement, depth + 2);
                print_block(itl,switch_case.block, depth + 2);
            }

            if(switch_node->default_statement)
            {
                const auto& default_case = *switch_node->default_statement;
                print_internal(itl,default_case.statement, depth + 2);
                print_block(itl,default_case.block, depth + 2);
            }

            break;
        }


        case ast_type::if_t:
        {
            IfNode* if_node = (IfNode*)root;
            printf("If stmt\n");

            print_if_stmt(itl,if_node->if_stmt,"If", depth);

            for(auto& else_if_stmt : if_node->else_if_stmt)
            {
                print_if_stmt(itl,else_if_stmt, "Else if", depth);
            }

            print_depth(depth + 1);
            printf("Else\n");
            print_block(itl,&if_node->else_stmt, depth + 2);

            break;
        }

        case ast_type::while_t:
        {
            WhileNode* while_node = (WhileNode*)root;

            printf("While\n");
            print_internal(itl,while_node->expr, depth + 1);
            print_block(itl,&while_node->block, depth + 2);
            break;
        }

        case ast_type::const_assert:
        {
            print_unary(itl,(ConstAssert*)root,"Const assert", depth);
            break;
        }

        case ast_type::function:
        {
            FuncNode* func = (FuncNode*)root;

            printf("Function %s(%x), va_args: %s\n",func->name.buf,func->attr_flags,func->args_name.buf);

            for(DeclNode* decl : func->args)
            {
                print_internal(itl,(AstNode*)decl, depth + 1);
            }

            print_block(itl,&func->block, depth + 2);

            for(TypeNode* type : func->return_type)
            {
                print_internal(itl,(AstNode*)type, depth + 1);
            }

            break;
        }

        case ast_type::ret:
        {
            RetNode* ret = (RetNode*)root;
            printf("Ret\n");

            for(AstNode* expr : ret->expr)
            {
                print_internal(itl,expr, depth + 1);
            }
            break;
        }

    }
}

void print(Interloper& itl, const AstNode *root)
{
    print_internal(itl,root,0);
}


std::pair<u32,u32> get_line_info(const String& filename, u32 idx)
{
    auto [file,err] = read_str_buf(filename);

    if(err)
    {
        printf("file %s does not exist\n",filename.buf);
        return std::pair{1,1};
    }

    // 1 indexed
    u32 col = 1;
    u32 row = 1;

    for(u32 i = 0; i < file.size && i != idx; i++)
    {
        if(file[i] == '\n')
        {
            row += 1;
            col = 0;
        }

        else
        {
            col += 1;
        }
    }

    destroy_arr(file);

    return std::pair{row,col};
}

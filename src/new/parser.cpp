#include <interloper.h>
#include <unistd.h>

Result<BlockNode*,parse_error> block(Parser &parser);
ParserResult block_ast(Parser &parser);
Option<ParserResult> try_parse_slice(Parser& parser, const Token& t);
Result<FuncNode*,parse_error> parse_func_sig(Parser& parser, const String& func_name,const Token& token);

static constexpr u32 ATTR_NO_REORDER = (1 << 0);
static constexpr u32 ATTR_FLAG = (1 << 1);
static constexpr u32 ATTR_USE_RESULT = (1 << 2);

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
#include "parser/type.cpp"
#include "parser/variable.cpp"
#include "parser/control_flow.cpp"
#include "parser/function.cpp"


ParserResult const_assert(Parser& parser,const Token& t)
{
    const auto left_paren_err = consume(parser,token_type::left_paren);
    if(!!left_paren_err)
    {
        return *left_paren_err;
    }

    const auto expr = expr_terminate(parser,"const_assert",token_type::right_paren);
    const auto right_paren_err = consume(parser,token_type::semi_colon);
    if(!!right_paren_err)
    {
        return *right_paren_err;
    }

    return ast_unary(parser,expr,ast_type::const_assert,t);       
}


ParserResult statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        case token_type::ret:
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

                    AstNode* initializer = *initializer_res;
                    initializer->type = ast_type::struct_return;

                    return initializer;
                }

                RecordNode* record = (RecordNode*)ast_record(parser,ast_type::ret,t);
                b32 done = false;

                // can be more than one expr (comma seperated)
                while(!done)
                {
                    auto list_res = expr_list(parser,"return",token_type::semi_colon,&done);
                    if(!list_res)
                    {
                        return list_res.error();
                    }

                    push_var(record->nodes,*list_res);
                }

                return (AstNode*)record;
            }

            else
            {
                const auto term_err = consume(parser,token_type::semi_colon);
                if(!!term_err)
                {
                    return *term_err;
                }
                return ast_plain(parser,ast_type::ret,t);
            }
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
                        AstNode* left = ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok);

                        return ast_binary(parser,left,parse_struct_initializer(parser),ast_type::equal,sym_tok);  
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

            return block_ast(parser);
        }

        // assume one cond for now
        case token_type::for_t:
        {
            return parse_for(parser);
        }

        case token_type::while_t:
        {
            return parse_while(parser);
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            return parse_if(parser);
        }

        case token_type::switch_t:
        {
            return parse_switch(parser);
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
    if(!!left_paren_err)
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
    if(!!right_paren_err)
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
                if(!!struct_decl_err)
                {
                    return struct_decl_err;
                }
                break;
            }

            case token_type::enum_t:
            {
                (void)consume(parser,token_type::enum_t);

                const auto enum_decl_err = enum_decl(itl,parser,flags);
                if(!!enum_decl_err)
                {
                    return enum_decl_err;
                } 
                break; 
            }

            case token_type::func:
            {
                (void)consume(parser,token_type::func);

                const auto func_err = func_decl(itl,parser,flags);
                if(!!func_err)
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
            if(!!scope_err)
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
                if(!!lt_err)
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
                if(!!gt_err)
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
            if(!!func_err)
            {
                return func_err;
            }
            break;
        }

        case token_type::struct_t:
        {
            const auto struct_err = struct_decl(itl,parser);
            if(!!struct_err)
            {
                return struct_err;
            }
            break;
        }

        case token_type::enum_t:
        {
            const auto enum_err = enum_decl(itl,parser,0);
            if(!!enum_err)
            {
                return enum_err;
            }
            break;
        }

        case token_type::type_alias:
        {
            const auto type_err = type_alias(itl,parser);
            if(!!type_err)
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

            parser.cur_namespace = scan_namespace(itl.global_namespace,name_space);

            // Namespace does not allready exist create it!
            if(!parser.cur_namespace)
            {
                parser.cur_namespace = new_named_scope(*parser.namespace_allocator,*parser.global_string_allocator,parser.global_namespace,name_space);
            }

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
            if(!!hash_err)
            {
                destroy_parser(parser);
                return hash_err;
            }

            const auto directive_err = parse_directive(itl,parser);
            if(!!directive_err)
            {
                destroy_parser(parser);
                return directive_err;
            }
        }

        // plain decl
        else
        {
            const auto parse_err = parse_top_level_token(itl,parser,queue);
            if(!!parse_err)
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

        if(!!err)
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

void print(const AstNode *root, b32 override_seperator)
{
    static int depth = 0;

    if(!root)
    {
        print_depth(depth + 1);
        puts("EMPTY");
        return;
    }


    if(!override_seperator && (root->type == ast_type::function || root->type == ast_type::struct_t || root->type == ast_type::enum_t))
    {
        printf("\n\n\n");
    }


    depth += 1;

    print_depth(depth);

    switch(root->fmt)
    {
        case ast_fmt::plain:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);
            break;
        }

        case ast_fmt::arith_unary_op:
        {
            ArithUnaryNode* unary_node = (ArithUnaryNode*)root;
            printf(" %s\n",ARITH_UNARY_NAMES[u32(unary_node->oper)]);

            print(unary_node->expr);
            break;
        }

        case ast_fmt::arith_bin_op:
        {
            ArithBinNode* bin_node = (ArithBinNode*)root;
            printf(" %s\n",ARITH_BIN_NAMES[u32(bin_node->oper)]);

            print(bin_node->left);
            print(bin_node->right);
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

            if(value_node->value.sign)
            {
                printf("value: %ld\n",s64(value_node->value.v));
            }

            else
            {
                printf("value: %lu\n",value_node->value.v);
            }
            break;
        }

        case ast_fmt::float_t:
        {
            FloatNode* float_node = (FloatNode*)root;

            printf("float: %lf\n",float_node->value);
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

            if(func_node->block)
            {
                print((AstNode*)func_node->block);
            }

            for(u32 r = 0; r < count(func_node->return_type); r++)
            {
                print((AstNode*)func_node->return_type[r]);
            }
            break;
        }

        case ast_fmt::designated_initializer_list:
        {
            DesignatedListNode* list = (DesignatedListNode*) root;

            printf("designated initializer list\n");

            for(auto& initializer : list->initializer)
            {
                printf("%s",initializer.name.buf);
                print(initializer.expr);
            }

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

        case ast_fmt::enum_t:
        {
            EnumNode* enum_node = (EnumNode*) root;

            print((AstNode*)enum_node->type);            
            for(u32 m = 0; m < count(enum_node->member); m++)
            {
                // we store this directly as its not used anywher else
                // fake the depth print
                depth += 1;
                print_depth(depth);

                printf("member %s: \n",enum_node->member[m].name.buf);
                print(enum_node->member[m].initializer);
                depth -= 1;
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
            // does double duty with sizeof_type
            printf(" %s: %s %s\n", AST_NAMES[static_cast<size_t>(root->type)],type_decl->is_const? "const" : "",type_decl->name.buf);

            if(type_decl->func_type)
            {
                print((AstNode*)type_decl->func_type,true);
            }

            for(u32 c = 0; c < count(type_decl->compound_type); c++)
            {
                print(type_decl->compound_type[c]);
            }
            break;
        }

        case ast_fmt::declaration:
        {
            DeclNode* decl_node = (DeclNode*)root;

            printf("%s : %s\n",decl_node->node.type == ast_type::const_decl? "const decl" : "decl",decl_node->name.buf);

            print((AstNode*)decl_node->type);
            print(decl_node->expr);
            break;
        }

        case ast_fmt::auto_decl:
        {
            AutoDeclNode* auto_decl = (AutoDeclNode*)root;
            printf("auto decl : %s\n",auto_decl->name.buf);
            print(auto_decl->expr);
            break;
        }

        case ast_fmt::global_declaration:
        {
            GlobalDeclNode* global_node = (GlobalDeclNode*)root;
            printf("global %s decl:\n",global_node->decl->node.type == ast_type::const_decl ? "const" : "");
            print((AstNode*)global_node->decl);
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

            printf("function call\n");

            print(func_call->expr);

            for(u32 a = 0; a < count(func_call->args); a++)
            {
                print(func_call->args[a]);
            }                                    

            break;
        }

        case ast_fmt::for_iter:
        {
            printf("for\n");

            ForIterNode* for_node = (ForIterNode*)root;

            print(for_node->initializer);
            print(for_node->cond);
            print(for_node->post);

            print((AstNode*)for_node->block);

            break;
        }

        case ast_fmt::for_range:
        {
            ForRangeNode* for_node = (ForRangeNode*)root;

            printf("for [%s, %s] in\n",for_node->name_one.buf, for_node->name_two.buf);
            print(for_node->cond);

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

        case ast_fmt::struct_initializer:
        {
            StructInitializerNode* struct_initializer_node = (StructInitializerNode*)root;

            printf("%s %s\n",AST_NAMES[u32(struct_initializer_node->node.type)],struct_initializer_node->struct_name.buf);
            print(struct_initializer_node->initializer);
            break;
        }

        case ast_fmt::index:
        {
            IndexNode* index_node = (IndexNode*)root;

            printf("index: %s\n",index_node->name.buf);

            for(u32 i = 0; i < count(index_node->indexes); i++)
            {
                print(index_node->indexes[i]);
            }

            break;
        }

        case ast_fmt::slice:
        {
            SliceNode* slice_node = (SliceNode*)root;

            printf("slice: %s\n",slice_node->name.buf);

            if(slice_node->lower)
            {
                print(slice_node->lower);
            }

            if(slice_node->upper)
            {
                print(slice_node->upper);
            }
            break;
        }

        case ast_fmt::switch_t:
        {
            printf("switch\n");

            SwitchNode* switch_node = (SwitchNode*)root;

            print((AstNode*)switch_node->expr);

            for(u32 s = 0; s < count(switch_node->statements); s++)
            {
                print((AstNode*)switch_node->statements[s]);
            }

            print((AstNode*)switch_node->default_statement);

            break;
        }

        case ast_fmt::case_t:
        {
            printf("case\n");

            CaseNode* case_node = (CaseNode*)root;

            print((AstNode*)case_node->statement);
            print((AstNode*)case_node->block);

            break;
        }

        case ast_fmt::scope:
        {
            printf("scope: ");

            ScopeNode* scope_node = (ScopeNode*)root;

            for(size_t i = 0; i < count(scope_node->scope); i++)
            {
                printf("%s::",scope_node->scope[i].buf);
            }
            putchar('\n');

            print(scope_node->expr);

            break;
        }

        case ast_fmt::tuple_assign:
        {
            puts("tuple assign");

            TupleAssignNode* tuple_node = (TupleAssignNode*)root;

            for(u32 s = 0; s < count(tuple_node->symbols); s++)
            {
                print(tuple_node->symbols[s]);
            }

            print((AstNode*)tuple_node->func_call);
            break;
        }

        case ast_fmt::type_alias:
        {
            AliasNode* alias_node = (AliasNode*)root;

            printf("type alias %s\n",alias_node->name.buf);

            print((AstNode*)alias_node->type);
            break;
        }

        case ast_fmt::builtin_type_info:
        {
            BuiltinAccessNode* builtin_node = (BuiltinAccessNode*)root;

            printf("builtin type access: %s.%s\n",TYPE_NAMES[u32(builtin_node->type)],builtin_node->field.buf);
            break;
        }
    }

    depth -= 1;
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

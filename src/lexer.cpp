#include <lexer.h>


void print_tokens(const Array<Token> &tokens)
{
    for(u32 t = 0; t < count(tokens); t++)
    {
        print_token(tokens[t]);
    }
}


char peek(u32 offset, const String& file)
{
    return offset < file.size? file[offset] : '\0';
}

void insert_token(Lexer &lexer, token_type type)
{
    push_var(lexer.tokens,token_plain(type,lexer.row,lexer.column));
}


void insert_token(Lexer &lexer, token_type type, u32 col)
{
    push_var(lexer.tokens,token_plain(type,lexer.row,col));
}

void insert_token(Lexer &lexer, token_type type, const String &literal, u32 col)
{
    push_var(lexer.tokens,token_literal(type,literal,lexer.row,col));
}

void insert_token_char(Lexer& lexer,char c, u32 col)
{
    push_var(lexer.tokens,token_char(c,lexer.row,col));
}

void insert_token_value(Lexer& lexer,const Value& value, u32 col)
{
    push_var(lexer.tokens,token_value(value,lexer.row,col));
}


template<typename F>
bool read_immediate_internal(String& literal, u32 offset, F lambda)
{
    for(u32 i = offset; i < literal.size; i++)
    {
        // valid part of the value
        if(lambda(literal[i]))
        {
            continue;
        }

        // values cannot have these at the end!
        else if(isalpha(literal[i]))
        {
            return false;
        }

        // we have  < ; + , etc stop parsing
        else 
        {
            // clamp string to the actual literal length 
            literal.size = i - 1;
            break;
        }
    }

    return true;
}


bool read_immediate(String& literal)
{
    // an empty immediate aint much use to us
    if(!literal.size)
    {
        return true;
    }

    u32 offset = 0;

    const auto c = literal[0];

    // allow - or +
    if(c == '-' || c == '+')
    {
        offset = 1;
        // no digit after the sign is of no use
        if(literal.size == 1)
        {
            return true;
        }
    }

    bool valid = false;

    // verify we have a valid hex number
    if(string_equal(string_slice(literal,offset,2),"0x"))
    {
        // skip past the prefix
        offset += 2;
        valid = read_immediate_internal(literal,offset,[](const char c) 
        {
            return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
        });
    }

    // verify its ones or zeros
    else if(string_equal(string_slice(literal,offset,2),"0x"))
    {
        // skip past the prefix
        offset += 2;                
        valid = read_immediate_internal(literal,offset,[](const char c) 
        {
            return c == '0' || c == '1';
        });
    }

    // verify we have all digits
    else
    {
        valid = read_immediate_internal(literal,offset,[](const char c) 
        {
            return c >= '0' && c <= '9';
        });
    }
    

    return !valid;
}

void advance(Lexer& lexer, s32 v = 1)
{
    lexer.column += v;
    lexer.idx += v;
}

// TODO: make sure the number fits in 32 bits
u32 convert_imm(const String& str)
{
    return strtol(str.buf,NULL,0);
}



// true on error
bool decode_imm(Lexer &lexer,const String& file)
{
    const u32 start_idx = lexer.idx;
    const u32 start_col = lexer.column;

    // get back out a literal for parsing
    // TODO: make this use slicing on the string
    String literal = string_slice(file,lexer.idx,file.size - lexer.idx);
    const bool error = read_immediate(literal);

    if(error)
    {
        return true;
    }

    // get the value from the string
    const u32 v = convert_imm(literal);
    const bool sign = file[start_idx] == '-';

    const Value value = {v,sign};

    insert_token_value(lexer,value,start_col);

    // ignore the terminator
    advance(lexer,literal.size);

    return false; 
}

#include "keyword_hashtable.cpp"


s32 keyword_lookup(const String& name)
{
    return lookup_internal_hashtable(KEYWORD_TABLE,KEYWORD_TABLE_SIZE,name);
}

void destroy_lexer(Lexer& lexer)
{
    destroy_arr(lexer.tokens);
}

void panic(Lexer& lexer, const String& filename, const char* fmt, ...)
{
    printf("lexer error: %s %d:%d: ",filename.buf,lexer.row+1,lexer.column+1);

    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);

    putchar('\n');

    print_line(filename,lexer.row+1);   
}

// TODO: change file to be a string, and just conv them all in
b32 tokenize(const String& file,const String& file_name,ArenaAllocator* string_allocator, Array<Token>& tokens_out)
{
    Lexer lexer;

    lexer.row = 0;
    lexer.column = 0;
    lexer.string_allocator = string_allocator; 

    const auto size = file.size;
    for(lexer.idx = 0; lexer.idx < size; advance(lexer))
    {
        auto c = file[lexer.idx];

        if(lexer.in_comment)
        {
            if(c == '\n')
            {
                lexer.column = -1;
                lexer.row++;                
            }

            else if(c == '*' && peek(lexer.idx+1,file) == '/')
            {
                lexer.in_comment = false;
                advance(lexer);
            }

            continue;
        }

        switch(c)
        {
            case '\t': break;
            case ' ': break;
            case '\r': break;
            case '\n':
            {
                lexer.column = -1;
                lexer.row++;
                break;
            }

            case '(': insert_token(lexer,token_type::left_paren); break;

            case ')': insert_token(lexer,token_type::right_paren); break;

            case '{': insert_token(lexer,token_type::left_c_brace); break;

            case '}': insert_token(lexer,token_type::right_c_brace); break;

            case ',': insert_token(lexer,token_type::comma); break;

            case '=': 
            {
                // equal
                if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::logical_eq);
                    advance(lexer);
                }
                
                else
                {
                    insert_token(lexer,token_type::equal); 
                }

                
                break;
            }


            // char literal
            case '\'':
            {
                const u32 col = lexer.column;

                const char c = peek(lexer.idx+1,file);

                if(c == '\0')
                {
                    destroy_lexer(lexer);
                    panic(lexer,file_name,"eof hit in middle of char literal");
                    return true;
                }

                if(peek(lexer.idx+2,file) != '\'')
                {
                    panic(lexer,file_name,"unterminated char literal");
                    destroy_lexer(lexer);
                    return true;
                }

                insert_token_char(lexer,c,col);

                advance(lexer,2);
                break;
            }

            // string literal
            case '\"':
            {
                const u32 start_col = lexer.column;
                advance(lexer);


                StringBuffer buffer;

                while(lexer.idx < size)
                {  
                    char c = file[lexer.idx];

                    // escape sequence
                    if(c == '\\')
                    {
                        char e = peek(lexer.idx + 1,file);

                        switch(e)
                        {
                            // filefeed
                            case 'n':
                            {
                                c = '\n';
                                advance(lexer);
                                break;
                            }

                            default:
                            {
                                destroy_lexer(lexer);
                                panic(lexer,file_name,"unterminated char literal");
                                return true;
                            }
                        }
                    }

                    else if(c == '\"')
                    {
                        break;
                    }

                    advance(lexer);
                    push_char(*lexer.string_allocator,buffer,c);
                }

                // null term the string
                push_char(*lexer.string_allocator,buffer,'\0');

                // create string fomr the array
                String literal = make_string(buffer);

                insert_token(lexer,token_type::string,literal,start_col);
                break;
            }


            case '[': insert_token(lexer,token_type::sl_brace); break;

            case ']': insert_token(lexer,token_type::sr_brace); break;

            case '.': insert_token(lexer,token_type::dot); break;
            
            case '?': insert_token(lexer,token_type::qmark); break;

            case ';': insert_token(lexer,token_type::semi_colon); break;

            case ':': 
            {
                if(peek(lexer.idx+1,file) == ':')
                {
                    insert_token(lexer,token_type::scope);
                    advance(lexer);
                }

                else 
                {
                    insert_token(lexer,token_type::colon); 
                }
                break;
            }

            case '@': insert_token(lexer,token_type::deref); break;

            case '*':
            { 
                if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::times_eq);
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::times); 
                }
                break;
            }        

            case '+':
            {
                if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::plus_eq);
                    advance(lexer);
                }

                else if(peek(lexer.idx+1,file) == '+')
                {
                    insert_token(lexer,token_type::increment);
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::plus); 
                }
                break;
            }

            case '-': 
            {
                if(peek(lexer.idx+1,file) == '-')
                {
                    insert_token(lexer,token_type::decrement);
                    advance(lexer);
                }


                // parse out negative literal
                else if(isdigit(peek(lexer.idx+1,file)))
                {
                    if(decode_imm(lexer,file))
                    {
                        destroy_lexer(lexer);
                        panic(lexer,file_name,"malformed integer literal");
                        return true;
                    }
                }

                else if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::minus_eq);
                    advance(lexer);
                }                

                else
                {
                    insert_token(lexer,token_type::minus);
                }
                break;
            }
            

            case '&':
            {
                // equal
                if(peek(lexer.idx+1,file) == '&')
                {
                    insert_token(lexer,token_type::logical_and);
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::operator_and);
                }
                break;              
            }

            case '|':
            {
                // logical or
                if(peek(lexer.idx+1,file) == '|')
                {
                    insert_token(lexer,token_type::logical_or);
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::bitwise_or);
                }
                break;              
            }

            case '~': insert_token(lexer,token_type::bitwise_not); break;
            case '^': insert_token(lexer,token_type::bitwise_xor); break;

            case '!':
            {
                // not equal
                if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::logical_ne);
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::logical_not);
                }
                break;
            }

            case '<':
            {
                if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::logical_le);
                    advance(lexer);
                }

                else if(peek(lexer.idx+1,file) == '<')
                {
                    insert_token(lexer,token_type::shift_l);
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::logical_lt);
                }
                break;
            }

            case '>':
            {
                if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::logical_ge);
                    advance(lexer);
                }

                else if(peek(lexer.idx+1,file) == '>')
                {
                    insert_token(lexer,token_type::shift_r);
                    advance(lexer);
                }


                else
                {
                    insert_token(lexer,token_type::logical_gt);
                }
                break;
            }

            case '%': insert_token(lexer,token_type::mod); break;

            case '/': 
            {
                // we have comment eat tokens until a newline
                if(peek(lexer.idx+1,file) == '/')
                {
                    while(lexer.idx < size)
                    {
                        if(file[lexer.idx] == '\n')
                        {
                            lexer.column = -1;
                            lexer.row++;
                            break;
                        }

                        advance(lexer);
                    }
                }

                else if(peek(lexer.idx+1,file) == '=')
                {
                    insert_token(lexer,token_type::divide_eq);
                    advance(lexer);
                }

                // start of multifile comment
                else if(peek(lexer.idx+1,file) == '*')
                {
                    lexer.in_comment = true;
                    advance(lexer);
                }

                else
                {
                    insert_token(lexer,token_type::divide);
                } 
                break;
            }

            default:
            {
                const u32 start_idx = lexer.idx;
                const u32 start_col = lexer.column;

                // potential symbol
                if(isalpha(c) || c == '_')
                {
                    while(lexer.idx < size)
                    {
                        advance(lexer);
                        const char x = file[lexer.idx];
                        if(!isalnum(x) && x != '_')
                        {
                            advance(lexer,-1);
                            break;
                        }
                    }

                    String literal = copy_string(*lexer.string_allocator,string_slice(file,start_idx,(lexer.idx - start_idx) + 1));


                    const s32 slot = keyword_lookup(literal);

                    // if its a keyword identify its type
                    // else its a symbol
                    if(slot != INVALID_SLOT)
                    {
                        insert_token(lexer,KEYWORD_TABLE[slot].v,start_col);
                    }

                    else
                    {
                        insert_token(lexer,token_type::symbol,literal,start_col);
                    }

                }

                // parse out a integer literal
                // we will ignore floats for now
                // 0b
                // 0x
                // 0
                else if(isdigit(c))
                {
                    if(decode_imm(lexer,file))
                    {
                        destroy_lexer(lexer);
                        panic(lexer,file_name,"malformed integer literal");
                        return true;
                    }
                }

                else
                {
                    panic(lexer,file_name,"unexpected char '%c",c);
                    destroy_lexer(lexer);
                    return true;
                }
                break;
            }
        }
    }


    tokens_out = lexer.tokens;
    return false;
}

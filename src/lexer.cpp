#include <lexer.h>


void print_tokens(const Array<Token> &tokens)
{
    for(u32 t = 0; t < count(tokens); t++)
    {
        print_token(tokens[t]);
    }
}


void insert_token(Lexer &lexer, token_type type)
{
    push_var(lexer.tokens,token_plain(type,lexer.idx));
}


void insert_token_float(Lexer& lexer, f64 value)
{
    push_var(lexer.tokens,token_float(value,lexer.idx));
}

void insert_token(Lexer &lexer, token_type type, const String &literal)
{
    push_var(lexer.tokens,token_literal(type,literal,lexer.idx));
}

void insert_token_char(Lexer& lexer,char c)
{
    push_var(lexer.tokens,token_char(c,lexer.idx));
}

void insert_token_value(Lexer& lexer,const Value& value)
{
    push_var(lexer.tokens,token_value(value,lexer.idx));
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
    lexer.error = true;
    auto [row,col] = get_line_info(filename,lexer.idx);
    printf("lexer error: %s %d:%d: ",filename.buf,row,col);

    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);

    putchar('\n');

    print_line(filename,row);   
}

char escape_char(Lexer& lexer, const String& file_name,char escape_char)
{
    switch(escape_char)
    {
        case 'n': return '\n';
        case '0': return '\0';
        case '\\': return '\\';

        default:
        {
            destroy_lexer(lexer);
            panic(lexer,file_name,"unknown char literal '%c'",escape_char);
            return '\0';
        }
    } 
}

std::pair<Value,b32> parse_value(const char** src_ptr)
{
    const char* src = *src_ptr;

    // default to base 10
    u32 base = 10;
    Value value;

    // check sign
    if(*src == '-')
    {
        value.sign = true;
        src++;
    }

    // peek for base
    if(*src == '0')
    {
        if(src[1] == 'x')
        {
            base = 16;
            src += 2;
        }

        else if(src[1] == 'b')
        {
            base = 2;
            src += 2;
        }
    }

    b32 done = false;

    while(!done)
    {
        const char c = toupper(*src);
 
        // ignore _
        if(c == '_')
        {
            src++;
            continue;
        }

        u64 v = 0;

        // convert the digit
        if(base >= 2 && c >= '0' && c <= '1')
        {
            v = c - '0';
        }

        else if(base >= 10 && c >= '2' && c <= '9')
        {
            v = c - '0';
        }

        else if(base >= 16 && c >= 'A' && c <= 'F')
        {
            v = 10 + (c - 'A');
        }

        else
        {
            // alpha in this context is an error
            if(isalpha(c))
            {
                return std::pair{value,true};
            }

            else
            {
                done = true;
                break;                
            }
        }

        // acummulate digit
        value.v *= base;
        value.v += v;

        src++;
    }

    //printf("parsed value: %s0x%lx\n",value.sign? "-" : "",value.v);

    // make twos complement
    if(value.sign)
    {
        value.v = (~value.v) + s64(1);
    }

    *src_ptr = src;
    return std::pair{value,false};
}

#include "lexer_lut.cpp"

// based upon https://nothings.org/computer/lexing.html
b32 tokenize(const String& file,const String& file_name,ArenaAllocator* string_allocator, Array<Token>& tokens_out)
{
    Lexer lexer;

    lexer.string_allocator = string_allocator; 

    const char* src = file.buf;
    
    for(;;)
    {
        u32 len = 0;
        u32 state = LEX_STATE_START;

        // still need to parse token
        while(state >= LEX_STATE_ACTIVE)
        {
            const u32 active_idx = state - LEX_STATE_ACTIVE;
            len += state != LEX_STATE_START;

            const char c = *src++;
            lex_class lc = LEX_CLASS[u32(c)];
            state = LEX_ACTIVE_STATES[active_idx][u32(lc)];
        }

        const char* start = (src - (len + 1));
        lexer.idx =  start - file.buf;

        // switch on end state to handle resolution of any multi char tokens
        switch(state)
        {
            case LEX_STATE_INVALID_CHAR:
            {
                src--;
                panic(lexer,file_name,"Invalid char '%c' : %d\n",*src,*src);
                destroy_lexer(lexer);
                return true;
            }

            case LEX_STATE_STRING_FIN:
            {
                // we skip '"'
            
                StringBuffer buffer;

                while(*src)
                {  
                    char c = *src++;

                    // escape sequence
                    if(c == '\\')
                    {
                        c = escape_char(lexer,file_name,*src);
                        src++;

                        if(lexer.error)
                        {
                            return true;
                        }
                    }

                    else if(c == '\"')
                    {
                        break;
                    }

                    push_char(*lexer.string_allocator,buffer,c);
                }

                // null term the string
                push_char(*lexer.string_allocator,buffer,'\0');

                // create string fomr the array
                String literal = make_string(buffer);

                insert_token(lexer,token_type::string,literal);
                break;
            }

            case LEX_STATE_FORWARD_SLASH:
            {
                const char peek = *src;

                // comment process tokens until newline
                if(peek == '/')
                {
                    src++;
                    while(*src)
                    {
                        const char c = *src++;
                        if(c == '\n')
                        {
                            break;
                        }
                    }
                }

                // multi line comment
                else if(peek == '*')
                {
                    b32 done = false;
                    src++;
                    while(!done)
                    {
                        const char c = *src++;
                        
                        if(!c)
                        {
                            done = true;
                        }

                        else if(c == '*' && src[0] == '/')
                        {
                            done = true;
                            src++;
                        }
                    }
                }

                else if(peek == '=')
                {
                    insert_token(lexer,token_type::divide_eq);
                    src++;
                }

                else
                {
                    insert_token(lexer,token_type::divide);
                }

                break;
            }

            case LEX_STATE_MISC_FIN:
            {
                token_type tok_type = LEX_TYPE[*start & 0x7f];
                insert_token(lexer,tok_type);
                break;
            }

            case LEX_STATE_INT_FIN:
            {
                const char* tmp = start;

                b32 sign = false;

                // scan ahead to see if there is a dp
                if(*tmp == '-')
                {
                    sign = true;
                    tmp++;
                }

                while(isdigit(*tmp))
                {
                    tmp++;
                }

                // we are dealing with a float
                if(*tmp == '.')
                {
                    tmp++;

                    // TODO: we want our own function that ignores _
                    f64 value = atof(start);

                    if(sign)
                    {
                        value = -value;
                    }

                    // find the end
                    while(isdigit(*tmp))
                    {
                        tmp++;
                    }

                    src = tmp;

                    insert_token_float(lexer,value);
                }

                // integer
                else
                {
                    src = start;

                    const auto [value,err] = parse_value(&src);

                    if(err)
                    {
                        panic(lexer,file_name,"Invalid integer literal\n");
                        destroy_lexer(lexer);
                        return true;
                    }

                    insert_token_value(lexer,value);
                }
                break;
            }

            case LEX_STATE_SYM_FIN:
            {
                // correct as we actually want the termination token
                // as it not encoded by the state unlike in operators
                src--;

                const String literal_file = string_slice(file,lexer.idx,len);

                const s32 slot = keyword_lookup(literal_file);

                // if its a keyword identify its type
                // else its a symbol
                if(slot != INVALID_HASH_SLOT)
                {
                    insert_token(lexer,KEYWORD_TABLE[slot].v);
                }

                else
                {
                    // need to copy literal as we ditch the file later
                    const String literal = copy_string(*lexer.string_allocator,literal_file);
                    //printf("%s\n",literal.buf);
                    insert_token(lexer,token_type::symbol,literal);
                }
                break;
            }
            
            case LEX_STATE_DOT:
            {
                if(*src == '.' && src[1] == '.')
                {
                    insert_token(lexer,token_type::va_args);
                    src += 2;
                }

                else
                {
                    insert_token(lexer,token_type::dot);
                }                 
                break;
            }

            case LEX_STATE_CHAR:
            {
                const char peek = *src;

                if(peek == '\0')
                {
                    destroy_lexer(lexer);
                    panic(lexer,file_name,"eof hit in middle of char literal");
                    return true;
                }

                // potential escape char
                else if(peek == '\\')
                {
                    const char e = escape_char(lexer,file_name,src[1]);

                    if(lexer.error)
                    {
                        return true;
                    }
                    
                    if(src[2] != '\'')
                    {
                        panic(lexer,file_name,"unterminated char literal");
                        destroy_lexer(lexer);
                        return true;
                    }

                    insert_token_char(lexer,e);
                    src += 3;
                }

                else
                {
                    // normal char
                    if(src[1] != '\'')
                    {
                        panic(lexer,file_name,"unterminated char literal");
                        destroy_lexer(lexer);
                        return true;
                    }

                    insert_token_char(lexer,peek);
                    src += 2;
                }
                break;
            }

            case LEX_STATE_EOF:
            {
                tokens_out = lexer.tokens;
                return false;            
            }

            default:
            {
                const u32 size = TOKEN_INFO[state].size;

                insert_token(lexer,token_type(state));
                src = start + size;
                break;
            }
        }
    }



}
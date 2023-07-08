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
    push_var(lexer.tokens,token_plain(type,lexer.idx));
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

        default:
        {
            destroy_lexer(lexer);
            panic(lexer,file_name,"unknown char literal '%c'",escape_char);
            return '\0';
        }
    } 
}

#include "lexer_lut.cpp"

// TODO: change file to be a string, and just conv them all in
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
            len += IN_TOKEN[active_idx];

            const char c = *src++;
            lex_class lc = LEX_CLASS[u32(c)];
            state = LEX_ACTIVE_STATES[active_idx][u32(lc)];
        }


        // go back as we have seen a terminating token
        src--;

        const char* start = (src - len);
        lexer.idx =  start - file.buf;

        // switch on end state to handle resolution of any multi char tokens
        switch(state)
        {
            case LEX_STATE_INVALID_CHAR:
            {
                panic(lexer,file_name,"Invalid char '%c' : %d\n",*src,*src);
                destroy_lexer(lexer);
                return true;
            }

            case LEX_STATE_STRING_FIN:
            {
                // skip "
                src++;
            
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
                const char peek = src[1];

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

                else if(peek == '=')
                {
                    assert(false);
                }

                else
                {
                    assert(false);
                }
                break;
            }

            case LEX_STATE_MISC_FIN:
            {
                token_type tok_type = LEX_TYPE[*src & 0x7f];
                insert_token(lexer,tok_type);
                break;
            }

            case LEX_STATE_INT_FIN:
            {
                assert(false);
                break;
            }

            case LEX_STATE_SYM_FIN:
            {
                const String literal_file = string_slice(file,lexer.idx,len);

                const s32 slot = keyword_lookup(literal_file);

                // if its a keyword identify its type
                // else its a symbol
                if(slot != INVALID_SLOT)
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

            case LEX_STATE_EQ:
            {
                assert(false);
                break;
            }

            case LEX_STATE_TIMES:
            {
                assert(false);
                break;
            }

            case LEX_STATE_PLUS:
            {
                assert(false);
                break;
            }

            case LEX_STATE_MINUS:
            {
                assert(false);
                break;
            }
        
            case LEX_STATE_OR:
            {
                assert(false);
                break;
            }

            case LEX_STATE_AND:
            {
                assert(false);
                break;
            }

            case LEX_STATE_NOT:
            {
                assert(false);
                break;
            }

            case LEX_STATE_GT:
            {
                assert(false);
                break;
            }

            case LEX_STATE_LT:
            {
                assert(false);
                break;
            }

            case LEX_STATE_CHAR:
            {
                assert(false);
                break;
            }

            case LEX_STATE_EOF:
            {
                tokens_out = lexer.tokens;
                return false;            
            }

            default: assert(false);
        }
    }



}

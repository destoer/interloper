#include <lexer.h>


char peek(u32 offset, const std::string &file)
{
    return offset < file.size()? file[offset] : '\0';
}

void insert_token(Lexer &lexer, token_type type)
{
    lexer.tokens.push_back(token_plain(type,lexer.row,lexer.column));
}


void insert_token(Lexer &lexer, token_type type, u32 col)
{
    lexer.tokens.push_back(token_plain(type,lexer.row,col));
}

void insert_token(Lexer &lexer, token_type type, const String &literal, u32 col)
{
    lexer.tokens.push_back(token_literal(type,literal,lexer.row,col));
}

void insert_token_char(Lexer& lexer,char c, u32 col)
{
    lexer.tokens.push_back(token_char(c,lexer.row,col));
}


template<typename F>
bool verify_immediate_internal(const char* literal, u32 &i, u32 len, F lambda)
{
    for(; i < len; i++)
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
            return true;
        }
    }

    return true;
}


s32 verify_immediate(const char* literal, u32 len)
{
    // an empty immediate aint much use to us
    if(!len)
    {
        return -1;
    }

    u32 i = 0;

    const auto c = literal[0];

    // allow - or +
    if(c == '-' || c == '+')
    {
        i = 1;
        // no digit after the sign is of no use
        if(len == 1)
        {
            return -1;
        }
    }

    bool valid = false;


    // verify we have a valid hex number
    if(strncmp(&literal[i],"0x",2) == 0)
    {
        // skip past the prefix
        i += 2;
        valid = verify_immediate_internal(literal,i,len - i,[](const char c) 
        {
            return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
        });
    }

    // verify its ones or zeros
    else if(strncmp(&literal[i],"0x",2) == 0)
    {
        // skip past the prefix
        i += 2;                
        valid = verify_immediate_internal(literal,i,len - i,[](const char c) 
        {
            return c == '0' || c == '1';
        });
    }

    // verify we have all digits
    else
    {
        valid = verify_immediate_internal(literal,i,len - i,[](const char c) 
        {
            return c >= '0' && c <= '9';
        });
    }
    

    if(!valid)
    {
        return -1;
    }

    return i;    
}

void advance(Lexer& lexer, s32 v = 1)
{
    lexer.column += v;
    lexer.idx += v;
}

/*

Value read_value(const Token &t)
{
    const u32 v = convert_imm(t.literal); 

    // is this literal a -ve?
    const bool sign = t.literal[0] == '-';

    return Value(v,sign);
}

*/

// true on error
bool decode_imm(Lexer &lexer,const std::string &file)
{
    const u32 start_col = lexer.column;

    UNUSED(start_col);

    const s32 len = verify_immediate(&file[lexer.idx],file.size() - lexer.idx);

    if(!len)
    {
        printf("invalid immediate: %s\n",file.c_str());
        return true;
    }

    assert(false);

    // TODO: convert to a value in place and insert it

    // ignore one for the termination char
    advance(lexer,len);

    return false; 
}




// NOTE: sadly we cant define a "constant" std::map
// do not modify this
// TODO: look into using a perfect hash function for this
std::unordered_map<std::string, token_type> keywords = 
{

    {tok_name(token_type::for_t), token_type::for_t},
    {tok_name(token_type::if_t),token_type::if_t},
    {tok_name(token_type::else_t),token_type::else_t},

    {tok_name(token_type::decl),token_type::decl},
    {tok_name(token_type::const_t),token_type::const_t},

    {tok_name(token_type::u8),token_type::u8},
    {tok_name(token_type::u16),token_type::u16},
    {tok_name(token_type::u32),token_type::u32},

    {tok_name(token_type::s8),token_type::s8},
    {tok_name(token_type::s16),token_type::s16},
    {tok_name(token_type::s32),token_type::s32},
    {tok_name(token_type::byte_t),token_type::byte_t},
    {tok_name(token_type::bool_t),token_type::bool_t},

    {tok_name(token_type::false_t),token_type::false_t},
    {tok_name(token_type::true_t),token_type::true_t},
    {tok_name(token_type::null_t),token_type::null_t},

    {tok_name(token_type::import),token_type::import},
    {tok_name(token_type::struct_t),token_type::struct_t},

    {tok_name(token_type::cast),token_type::cast},
    {tok_name(token_type::sizeof_t),token_type::sizeof_t},
    {tok_name(token_type::func),token_type::func},
    {tok_name(token_type::ret),token_type::ret}
};

bool is_keyword(const String &literal)
{
    return keywords.count(std_string(literal));
}

token_type keyword_token_type(const String &literal)
{
    return keywords[std_string(literal)];
}


bool tokenize(const std::string& file, std::vector<Token>& tokens_out)
{
    Lexer lexer;

    lexer.row = 0;
    lexer.column = 0;
    lexer.tokens.clear();

    const auto size = file.size();
    for(lexer.idx = 0; lexer.idx < file.size(); advance(lexer))
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
                    printf("hit eof before char literal");
                    return true;
                }

                if(peek(lexer.idx+2,file) != '\'')
                {
                    printf("unterminated char literal");
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

                // TODO: dont require copying this
                std::string str;

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
                                printf("unknown escape sequnce \\%c\n",e);
                                return true;
                            }
                        }
                    }

                    else if(c == '\"')
                    {
                        break;
                    }

                    advance(lexer);
                    str += c;
                }

                assert(false);
                String literal;

                insert_token(lexer,token_type::string,literal,start_col);
                break;
            }


            case '[': insert_token(lexer,token_type::sl_brace); break;

            case ']': insert_token(lexer,token_type::sr_brace); break;

            case '.': insert_token(lexer,token_type::dot); break;
            
            case '?': insert_token(lexer,token_type::qmark); break;

            case ';': insert_token(lexer,token_type::semi_colon); break;

            case ':': insert_token(lexer,token_type::colon); break;

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

                else
                {
                    insert_token(lexer,token_type::plus); 
                }
                break;
            }

            case '-': 
            {
                // parse out negative literal
                if(isdigit(peek(lexer.idx+1,file)))
                {
                    if(decode_imm(lexer,file))
                    {
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
                    insert_token(lexer,token_type::minus); break;
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
                u32 start_col = lexer.column;

                // potential symbol
                if(isalpha(c) || c == '_')
                {
                    while(lexer.idx < size)
                    {
                        advance(lexer);
                        char x = file[lexer.idx];
                        if(!isalnum(x) && x != '_')
                        {
                            advance(lexer,-1);
                            break;
                        }
                    }

                    // TODO:
                    assert(false);
                    String literal;


                    // if its a keyword identify its type
                    // else its a symbol
                    if(is_keyword(literal))
                    {
                        insert_token(lexer,keyword_token_type(literal),start_col);
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
                        return true;
                    }
                }

                else
                {
                    printf("unexpected char %c\n",c);
                    return true;
                }
                break;
            }
        }
    }


    tokens_out = std::move(lexer.tokens);
    return false;
}

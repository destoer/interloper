#include <interloper.h>


char peek(u32 offset, const std::string &line)
{
    return offset < line.size()? line[offset] : '\0';
}

void insert_token(Lexer &lexer, token_type type)
{
    lexer.tokens.push_back(Token(type,"",lexer.row,lexer.column));
}

void insert_token(Lexer &lexer, token_type type, const std::string &literal)
{
    lexer.tokens.push_back(Token(type,literal,lexer.row,lexer.column));
}



template<typename F>
bool verify_immediate_internal(const std::string &line, u32 &i, F lambda)
{
    const auto len = line.size();

    for(; i < len; i++)
    {
        // valid part of the value
        if(lambda(line[i]))
        {
            continue;
        }

        // values cannot have these at the end!
        else if(isalpha(line[i]))
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


bool verify_immediate(const std::string &line, std::string &literal)
{
    const auto len = line.size();

    // an empty immediate aint much use to us
    if(!len)
    {
        return false;
    }

    u32 i = 0;

    const auto c = line[0];

    // allow - or +
    if(c == '-' || c == '+')
    {
        i = 1;
        // no digit after the sign is of no use
        if(len == 1)
        {
            return false;
        }
    }

    bool valid = false;


    // have prefix + one more digit at minimum
    const auto prefix = i+2 < len?  line.substr(i,2) : "";

    // verify we have a valid hex number
    if(prefix == "0x")
    {
        // skip past the prefix
        i += 2;
        valid = verify_immediate_internal(line,i,[](const char c) 
        {
            return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
        });
    }

    // verify its ones or zeros
    else if(prefix == "0b")
    {
        // skip past the prefix
        i += 2;                
        valid = verify_immediate_internal(line,i,[](const char c) 
        {
            return c == '0' || c == '1';
        });
    }

    // verify we have all digits
    else
    {
        valid = verify_immediate_internal(line,i,[](const char c) 
        {
            return c >= '0' && c <= '9';
        });
    }
    

    if(valid)
    {
        literal = line.substr(0,i);
    }

    return valid;    
}

// true on error
bool decode_imm(Lexer &lexer,const std::string &line)
{
    std::string literal = "";


    const auto success = verify_immediate(line.substr(lexer.column),literal);

    if(!success)
    {
        printf("invalid immediate: %s\n",line.c_str());
        return true;
    }

    // ignore one for the termination char
    lexer.column += literal.size() - 1;

    insert_token(lexer,token_type::value,literal);   
    return false; 
}





// NOTE: sadly we cant define a "constant" std::map
// do not modify this
std::unordered_map<std::string, token_type> keywords = 
{

    {tok_name(token_type::for_t), token_type::for_t},
    {tok_name(token_type::if_t),token_type::if_t},
    {tok_name(token_type::else_t),token_type::else_t},

    {tok_name(token_type::decl),token_type::decl},

    {tok_name(token_type::u8),token_type::u8},
    {tok_name(token_type::u16),token_type::u16},
    {tok_name(token_type::u32),token_type::u32},

    {tok_name(token_type::s8),token_type::s8},
    {tok_name(token_type::s16),token_type::s16},
    {tok_name(token_type::s32),token_type::s32},
    {tok_name(token_type::bool_t),token_type::bool_t},

    {tok_name(token_type::false_t),token_type::false_t},
    {tok_name(token_type::true_t),token_type::true_t},

    {tok_name(token_type::cast),token_type::cast},
    {tok_name(token_type::func),token_type::func},
    {tok_name(token_type::ret),token_type::ret}
};

bool is_keyword(const std::string &literal)
{
    return keywords.count(literal);
}

token_type keyword_token_type(const std::string &literal)
{
    return keywords[literal];
}

bool tokenize_line(Lexer &lexer,const std::string &line)
{
    const auto size = line.size();
    for(lexer.column = 0; lexer.column < size; lexer.column++)
    {
        auto c = line[lexer.column];
        switch(c)
        {
            case '\t': break;
            case ' ': break;

            case '(': insert_token(lexer,token_type::left_paren); break;

            case ')': insert_token(lexer,token_type::right_paren); break;

            case '{': insert_token(lexer,token_type::left_c_brace); break;

            case '}': insert_token(lexer,token_type::right_c_brace); break;

            case ',': insert_token(lexer,token_type::comma); break;

            case '=': 
            {
                // equal
                if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::logical_eq);
                    lexer.column++;
                }
                
                else
                {
                    insert_token(lexer,token_type::equal); 
                }

                
                break;
            }

            case ';': insert_token(lexer,token_type::semi_colon); break;

            case '*':
            { 
                if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::times_eq);
                    lexer.column++;
                }

                else
                {
                    insert_token(lexer,token_type::times); 
                }
                break;
            }        

            case '+':
            {
                if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::plus_eq);
                    lexer.column++;
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
                if(isdigit(peek(lexer.column+1,line)))
                {
                    if(decode_imm(lexer,line))
                    {
                        return true;
                    }
                }

                else if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::minus_eq);
                    lexer.column++;
                }                

                else
                {
                    insert_token(lexer,token_type::minus); break;
                }
                break;
            }
            

            case '&':
            {
                // not equal
                if(peek(lexer.column+1,line) == '&')
                {
                    insert_token(lexer,token_type::logical_and);
                    lexer.column++;
                }

                else
                {
                    insert_token(lexer,token_type::bitwise_and);
                }
                break;              
            }

            case '|':
            {
                // not equal
                if(peek(lexer.column+1,line) == '|')
                {
                    insert_token(lexer,token_type::logical_or);
                    lexer.column++;
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
                if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::logical_ne);
                    lexer.column++;
                }

                else
                {
                    insert_token(lexer,token_type::logical_not);
                }
                break;
            }

            case '<':
            {
                if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::logical_le);
                    lexer.column++;
                }

                else if(peek(lexer.column+1,line) == '<')
                {
                    insert_token(lexer,token_type::shift_l);
                    lexer.column++;
                }

                else
                {
                    insert_token(lexer,token_type::logical_lt);
                }
                break;
            }

            case '>':
            {
                if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::logical_ge);
                    lexer.column++;
                }

                else if(peek(lexer.column+1,line) == '>')
                {
                    insert_token(lexer,token_type::shift_r);
                    lexer.column++;
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
                // we have comment this line is now done
                if(peek(lexer.column+1,line) == '/')
                {
                    return false;
                }

                else if(peek(lexer.column+1,line) == '=')
                {
                    insert_token(lexer,token_type::divide_eq);
                    lexer.column++;
                }

                else
                {
                    insert_token(lexer,token_type::divide);
                } 
                break;
            }

            default:
            {
                // potential symbol
                if(isalpha(c) || c == '_')
                {
                    std::string literal(1,c);
                    while(lexer.column < size)
                    {
                        c = line[++lexer.column];
                        if(!isalnum(c) && c != '_')
                        {
                            lexer.column--;
                            break;
                        }
                        literal += c;
                    }


                    // if its a keyword identify its type
                    // else its a symbol
                    if(is_keyword(literal))
                    {
                        // TODO: we need to check for the else keyword so we can join else if

                        insert_token(lexer,keyword_token_type(literal));
                    }

                    else
                    {
                        insert_token(lexer,token_type::symbol,literal);
                    }

                }

                // parse out a integer literal
                // we will ignore floats for now
                // 0b
                // 0x
                // 0
                else if(isdigit(c))
                {
                    if(decode_imm(lexer,line))
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

    return false;
}


bool tokenize(Lexer &lexer,const std::vector<std::string> &file)
{
    lexer.row = 0;
    lexer.column = 0;
    lexer.tokens.clear();

    for(const auto &line: file)
    {
        if(tokenize_line(lexer,line))
        {
            printf("%s",file[lexer.row].c_str());
            printf("\nat: line %d col %d\n",lexer.row,lexer.column);
            return true;
        }

        lexer.row += 1;
    }

    return false;
}

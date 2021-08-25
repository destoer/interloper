#include <interloper.h>


char Lexer::peek(u32 offset, const std::string &line)
{
    return column + offset < line.size()? line[column + offset] : '\0';
}

void Lexer::tokenize_line(const std::string &line, std::vector<Token> &tokens)
{
    const auto size = line.size();
    for(column = 0; column < size; column++)
    {
        auto c = line[column];
        switch(c)
        {
            case '\t': break;
            case ' ': break;

            case '(': insert_token(tokens,token_type::left_paren); break;

            case ')': insert_token(tokens,token_type::right_paren); break;

            case '{': insert_token(tokens,token_type::left_c_brace); break;

            case '}': insert_token(tokens,token_type::right_c_brace); break;

            case ',': insert_token(tokens,token_type::comma); break;

            case '=': 
            {
                // equal
                if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::logical_eq);
                    column++;
                }
                
                else
                {
                    insert_token(tokens,token_type::equal); 
                }

                
                break;
            }

            case ';': insert_token(tokens,token_type::semi_colon); break;

            case '*':
            { 
                if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::times_eq);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::times); 
                }
                break;
            }        

            case '+':
            {
                if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::plus_eq);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::plus); 
                }
                break;
            }

            case '-': 
            {
                // parse out negative literal
                if(isdigit(peek(1,line)))
                {
                    decode_imm(line,column,tokens);
                }

                else if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::minus_eq);
                    column++;
                }                

                else
                {
                    insert_token(tokens,token_type::minus); break;
                }
                break;
            }
            

            case '&':
            {
                // not equal
                if(peek(1,line) == '&')
                {
                    insert_token(tokens,token_type::logical_and);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::bitwise_and);
                }
                break;              
            }

            case '|':
            {
                // not equal
                if(peek(1,line) == '|')
                {
                    insert_token(tokens,token_type::logical_or);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::bitwise_or);
                }
                break;              
            }

            case '~': insert_token(tokens,token_type::bitwise_not); break;
            case '^': insert_token(tokens,token_type::bitwise_xor); break;

            case '!':
            {
                // not equal
                if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::logical_ne);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::logical_not);
                }
                break;
            }

            case '<':
            {
                if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::logical_le);
                    column++;
                }

                if(peek(1,line) == '<')
                {
                    insert_token(tokens,token_type::shift_l);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::logical_lt);
                }
                break;
            }

            case '>':
            {
                if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::logical_ge);
                    column++;
                }

                if(peek(1,line) == '>')
                {
                    insert_token(tokens,token_type::shift_r);
                    column++;
                }


                else
                {
                    insert_token(tokens,token_type::logical_gt);
                }
                break;
            }

            case '%': insert_token(tokens,token_type::mod); break;

            case '/': 
            {
                // we have comment this line is now done
                if(peek(1,line) == '/')
                {
                    return;
                }

                else if(peek(1,line) == '=')
                {
                    insert_token(tokens,token_type::divide_eq);
                    column++;
                }

                else
                {
                    insert_token(tokens,token_type::divide);
                } 
                break;
            }

            default:
            {
                // potential symbol
                if(isalpha(c) || c == '_')
                {
                    std::string literal(1,c);
                    while(column < size)
                    {
                        c = line[++column];
                        if(!isalnum(c) && c != '_')
                        {
                            column--;
                            break;
                        }
                        literal += c;
                    }


                    // if its a keyword identify its type
                    // else its a symbol
                    if(is_keyword(literal))
                    {
                        // TODO: we need to check for the else keyword so we can join else if

                        insert_token(tokens,keyword_token_type(literal));
                    }

                    else
                    {
                        insert_token(tokens,token_type::symbol,literal);
                    }

                }

                // parse out a integer literal
                // we will ignore floats for now
                // 0b
                // 0x
                // 0
                else if(isdigit(c))
                {
                    decode_imm(line,column,tokens);
                }

                else
                {
                    panic("unexpected char %c\n",c);
                    return;
                }
                break;
            }
        }
    }
}

std::vector<Token> Lexer::tokenize(const std::vector<std::string> *file)
{
    error = false;
    this->file = file;
    assert(file != nullptr);

    std::vector<Token> tokens;
    row = 0;
    for(const auto &line: *file)
    {
        tokenize_line(line,tokens);
        row += 1;
    }

    return tokens;
}

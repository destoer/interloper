#include <interloper.h>


void Lexer::tokenize_line(const std::string &line, std::vector<Token> &tokens)
{
    const auto size = line.size();
    for(size_t i = 0; i < size; i++)
    {
        auto c = line[i];
        switch(c)
        {
            case '\t': break;
            case ' ': break;
            
            case '\n':
            {
                // add line counting later
                //line += 1;
                break;
            }

            case '(': tokens.push_back(Token(token_type::left_paren)); break;

            case ')': tokens.push_back(Token(token_type::right_paren)); break;

            case '{': tokens.push_back(Token(token_type::left_c_brace)); break;

            case '}': tokens.push_back(Token(token_type::right_c_brace)); break;

            case '=': tokens.push_back(Token(token_type::equal)); break;

            case ';': tokens.push_back(Token(token_type::semi_colon)); break;

            case '*': tokens.push_back(Token(token_type::times)); break;        

            case '+': tokens.push_back(Token(token_type::plus)); break;      


            default:
            {
                // potential symbol
                if(isalpha(c))
                {
                    std::string literal(1,c);
                    while(i < size)
                    {
                        c = line[++i];
                        if(!isalnum(c))
                        {
                            i--;
                            break;
                        }
                        literal += c;
                    }


                    // if its a keyword identify its type
                    // else its a symbol
                    if(is_keyword(literal))
                    {
                        tokens.push_back(Token(keyword_token_type(literal)));
                    }

                    else
                    {
                        tokens.push_back(Token(token_type::symbol,literal));
                    }

                }

                // parse out a integer literal
                // we will ignore floats for now
                // 0b
                // 0x
                // 0
                else if(isdigit(c))
                {
                    decode_imm(line,i,tokens);
                }

                else
                {
                    // make this return an error token
                    printf("unexpected char %c\n",c);
                    print_tokens(tokens);
                    exit(1);
                }
                break;
            }
        }
    }
}

std::vector<Token> Lexer::tokenize(const std::vector<std::string> &file)
{
    std::vector<Token> tokens;

    for(const auto &line: file)
    {
        tokenize_line(line,tokens);
    }

    return tokens;
}

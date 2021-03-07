#include <interloper.h>


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

            case '=': insert_token(tokens,token_type::equal); break;

            case ';': insert_token(tokens,token_type::semi_colon); break;

            case '*': insert_token(tokens,token_type::times); break;        

            case '+': insert_token(tokens,token_type::plus); break;

            case '-': insert_token(tokens,token_type::minus); break;

            case '/': insert_token(tokens,token_type::divide); break;

            default:
            {
                // potential symbol
                if(isalpha(c))
                {
                    std::string literal(1,c);
                    while(column < size)
                    {
                        c = line[++column];
                        if(!isalnum(c))
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

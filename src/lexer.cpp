#include <interloper.h>

void print_tokens(const std::vector<Token> &tokens)
{
    for(const auto &t: tokens)
    {
        printf("type: %s\n",TOKEN_NAMES[static_cast<size_t>(t.type)]);
        printf("literal: %s\n",t.literal.c_str());
    }
}

std::vector<Token> Lexer::tokenize(const std::string &file)
{
    int line = 0;
    std::vector<Token> tokens;

    const auto size = file.size();
    for(size_t i = 0; i < size; i++)
    {
        auto c = file[i];
        switch(c)
        {
            case '\t': break;
            case ' ': break;
            
            case '\n':
            {
                line += 1;
                break;
            }

            default:
            {
                // potential symbol
                if(isalpha(c))
                {
                    std::string literal(1,c);
                    while(i < size)
                    {
                        c = file[++i];
                        if(!isalnum(c))
                        {
                            i--;
                            break;
                        }
                        literal += c;
                    }


                    // if its a keyword identify it as a type
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

                else
                {
                    // make this return an error token
                    printf("unexpected char at line %d: %c\n",line,c);
                    print_tokens(tokens);
                    exit(1);
                }
                break;
            }
        }
    }

    return tokens;
}

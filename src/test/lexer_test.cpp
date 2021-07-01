#include <interloper.h>
#include <test.h>

struct LexerTest
{

    LexerTest(const char *l,bool e,const std::vector<Token> &t) : line(l), error(e), tokens(t)
    {

    }

    const char *line;
    bool error;
    const std::vector<Token> tokens;
};


LexerTest LEXER_TESTS[] =
{
    {"2 + 2",false,
        {
            Token(token_type::value,"2"),
            Token(token_type::plus),
            Token(token_type::value,"2")
        }
    },

    {"(6242 * 2) + 5 - 6",false,
        {
            Token(token_type::left_paren),
            Token(token_type::value,"6242"),
            Token(token_type::times),
            Token(token_type::value,"2"),
            Token(token_type::right_paren),
            Token(token_type::plus),
            Token(token_type::value,"5"),
            Token(token_type::minus),
            Token(token_type::value,"6")
        }
    },

    // note here we only care the token stream comes out correctly
    // we dont really care if the program is well formed at this point
    {"{}();",false,
        {
            Token(token_type::left_c_brace),
            Token(token_type::right_c_brace),
            Token(token_type::left_paren),
            Token(token_type::right_paren),
            Token(token_type::semi_colon)
        }
    },

    {"func s32 main()",false,
        {
            Token(token_type::func),
            Token(token_type::s32),
            Token(token_type::symbol,"main"),
            Token(token_type::left_paren),
            Token(token_type::right_paren)
        }
    },

    {"return x;",false,
        {
            Token(token_type::ret),
            Token(token_type::symbol,"x"),
            Token(token_type::semi_colon)
        }
    },

    {"ans = square(2);",false,
        {
            Token(token_type::symbol,"ans"),
            Token(token_type::equal),
            Token(token_type::symbol,"square"),
            Token(token_type::left_paren),
            Token(token_type::value,"2"),
            Token(token_type::right_paren),
            Token(token_type::semi_colon)
        }
    },

    {"#",true,{}}

};

static constexpr uint32_t LEXER_TEST_SIZE = sizeof(LEXER_TESTS) / sizeof(LEXER_TESTS[0]);


void lexer_test()
{
    puts("\nrunning lexer tests\n");


    Lexer lexer;

    // workaround to force tests through the same interface
    std::vector<std::string> dummy(1);

    for(uint32_t i = 0; i < LEXER_TEST_SIZE; i++)
    {
        const auto &test = LEXER_TESTS[i];
        dummy[0] = test.line;
        const auto tokens = lexer.tokenize(&dummy);

        // lexer did or did not report an error when it should
        // or did not return enough tokens
        if(lexer.error != test.error || tokens.size() != test.tokens.size())
        {
            printf("fail[%d]: %s\n",i,test.line);
            return;
        }


        // check every last token matches
        for(size_t t = 0; t < tokens.size(); t++)
        {
            if(tokens[t] != test.tokens[t])
            {
                printf("fail[%d]: %s\n",i,test.line);
                return;                
            }
        }


        printf("pass[%d]: %s\n",i,test.line);
    }

    puts("\nlexer tests done\n");

}
#include <interloper.h>

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
            token_literal(token_type::value,"2"),
            token_plain(token_type::plus),
            token_literal(token_type::value,"2")
        }
    },

    {"(6242 * 2) + 5 - 6",false,
        {
            token_plain(token_type::left_paren),
            token_literal(token_type::value,"6242"),
            token_plain(token_type::times),
            token_literal(token_type::value,"2"),
            token_plain(token_type::right_paren),
            token_plain(token_type::plus),
            token_literal(token_type::value,"5"),
            token_plain(token_type::minus),
            token_literal(token_type::value,"6")
        }
    },

    // note here we only care the token stream comes out correctly
    // we dont really care if the program is well formed at this point
    {"{}();",false,
        {
            token_plain(token_type::left_c_brace),
            token_plain(token_type::right_c_brace),
            token_plain(token_type::left_paren),
            token_plain(token_type::right_paren),
            token_plain(token_type::semi_colon)
        }
    },

    {"func s32 main()",false,
        {
            token_plain(token_type::func),
            token_plain(token_type::s32),
            token_literal(token_type::symbol,"main"),
            token_plain(token_type::left_paren),
            token_plain(token_type::right_paren)
        }
    },

    {"return x;",false,
        {
            token_plain(token_type::ret),
            token_literal(token_type::symbol,"x"),
            token_plain(token_type::semi_colon)
        }
    },

    {"ans = square(2);",false,
        {
            token_literal(token_type::symbol,"ans"),
            token_plain(token_type::equal),
            token_literal(token_type::symbol,"square"),
            token_plain(token_type::left_paren),
            token_literal(token_type::value,"2"),
            token_plain(token_type::right_paren),
            token_plain(token_type::semi_colon)
        }
    },

    {"#",true,{}}

};

static constexpr uint32_t LEXER_TEST_SIZE = sizeof(LEXER_TESTS) / sizeof(LEXER_TESTS[0]);


void lexer_test()
{
    puts("\nrunning lexer tests\n");


    ArenaAllocator string_allocator = make_allocator(AST_ALLOC_DEFAULT_SIZE);

    for(uint32_t i = 0; i < LEXER_TEST_SIZE; i++)
    {
        const auto &test = LEXER_TESTS[i];


        std::vector<Token> tokens;

        const b32 lexer_error = tokenize(test.line,&string_allocator,tokens);

        // lexer did or did not report an error when it should
        // or did not return enough tokens
        if(lexer_error != test.error || tokens.size() != test.tokens.size())
        {
            printf("fail[%d]: %s\n",i,test.line);
            printf("test error %d : actual error %d\n",test.error,lexer_error);

            puts("output: ");
            print_tokens(tokens);

            puts("expected: ");
            print_tokens(test.tokens);
            
            destroy_allocator(string_allocator);
            return;
        }

        // check every last token matches
        for(size_t t = 0; t < tokens.size(); t++)
        {
            if(tokens[t] != test.tokens[t])
            {
                printf("fail[%d]: %s\n",i,test.line);

                puts("output: ");
                print_tokens(tokens);

                puts("expected: ");
                print_tokens(test.tokens);

                destroy_allocator(string_allocator);
                return;           
            }
        }


        printf("pass[%d]: %s\n",i,test.line);
    }

    destroy_allocator(string_allocator);

    puts("\nlexer tests done\n");
}
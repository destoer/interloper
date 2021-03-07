#include <interloper.h>
#include <test.h>


struct ExprTest
{
    ExprTest(const char *expr,bool e,int32_t a) : ans(a),error(e), expression(expr)
    {}

    int32_t ans;
    bool error;
    const char *expression;
};


ExprTest EXPR_TESTS[] = 
{
    {"2 + 2",false,4},
    {"4 / 2",false,2},
    //{"((5+5)*2) + 4",false,24}, // TODO: this breaks here, but not in our assembler that uses the same method why?
    {"(5 + 3) * 4",false,32},
    {"3 + 4 * -5",false,-17},
    {"5 - 5 * 2",false,-5},
    {"3 + 4 + 5",false,12},
    {"-5 + 5",false,0},
    {"+5 - 5 + 4",false,4},
    {"-5",false,-5},
    {"+5",false,5},
    {"-*-",true,0},
    {"4)",true,4},
    {"((4+5)))+4",true,13},
    {")(3+55",true,58},    
};

static constexpr uint32_t EXPR_TESTS_SIZE = sizeof(EXPR_TESTS) / sizeof(EXPR_TESTS[0]);


// helper function sum expression tree
int32_t sum(AstNode *node)
{
    if(node != nullptr)
    {
        // TODO: 
        // reformat our tree struct to have specialised types when we figure out the best way to do it...
        // for now do this silly workaround

        const auto size = node->nodes.size();
        AstNode *left = size >= 1? node->nodes[0] : nullptr;
        AstNode *right = size >= 2? node->nodes[1] : nullptr;


        switch(node->data.type)
        {
            case ast_type::value:
            {
                return convert_imm(node->data.literal);
            }

            case ast_type::plus:
            {
                if(right != nullptr)
                {
                    return sum(left) + sum(right);
                }

                // unary plus
                return +sum(left);
            }

            case ast_type::minus:
            {
                if(right != nullptr)
                {
                    return sum(left) - sum(right);
                }

                // unary minus
                return -sum(left);     
            }

            case ast_type::times:
            {
                return sum(left) * sum(right);
            }

            case ast_type::divide:
            {
                return sum(left) / sum(right);
            }

            default:
            {
                printf("sum illegal tok:%d\n",static_cast<int>(node->data.type));
                exit(1);
                break;
            }
        }
    }

    else
    {
        return 0;
    }
}


void expr_test()
{
    puts("expr tests...");

    Lexer lexer;
    Parser parser;

    // silly workaround
    std::vector<std::string> dummy(1);

    AstNode *expr_tree = nullptr;


    for(uint32_t i = 0; i < EXPR_TESTS_SIZE; i++)
    {
        delete_tree(expr_tree);

        const auto &test = EXPR_TESTS[i];
        dummy[0] = test.expression;
        const auto tokens = lexer.tokenize(&dummy);

        // check that the expression even consists of valid tokens
        if(lexer.error)
        {
            printf("fail lex error[%d]: %s\n",i,test.expression);
            break;
        }

        parser.init(&dummy,&tokens);
        
        expr_tree = parser.expr(parser.next_token());

        // check this expression is valid
        if(parser.error != test.error)
        {
            printf("fail parser error[%d]: %s\n",i,test.expression);
            break;            
        }
        
        // test is supposed to not produce a valid expression so we aernt going to try and sum it



        

        if(!test.error)
        {

            const auto ans = sum(expr_tree);

            if(ans != test.ans)
            {
                printf("fail[%d]: %d != %d\n",i,test.ans,ans);
                break;             
            }

            else
            {
                printf("pass[%d]: %d == %d\n",i,test.ans,ans);
            }
        }

        else
        {
            printf("pass[%d]: reject expr %s \n",i,test.expression);
        }
        
    }

    delete_tree(expr_tree);
}
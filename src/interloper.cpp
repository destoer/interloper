#include <interloper.h>

// how do we organise this so getting out our actual type enum aint a pain
// do we want this information allready on the tree for us and do some casting?
// or keep doing very annoying string lookups

// do we push the type on the ast into a format that is less of a pain to convert because we have
// effectively reobscured the type on the ast when there is really no need to do so
// we could just have an enum and then take the base off the thing  and then get it into an actual type enum 
// we use 


// either way the 2nd needs to be done and maybe dumped ontop of the first

Interloper::Interloper()
{

}

// scan the top level of the parse tree for functions
// and grab the entire signature
// we wont worry about the scope on functions for now as we wont have namespaces for a while
void Interloper::parse_function_declarations()
{
    for(const auto n : root->nodes)
    {
        const auto &node = *n;
        // unless its a function declaration we dont care
        if(node.type != ast_type::function)
        {
            continue;
        }

        printf(" %s : %s\n",AST_NAMES[static_cast<size_t>(node.type)],node.literal.c_str());

    /*   
        const auto return_type = node.nodes[0].type;
        const auto name = node.literal;
    
        // rip every arg
    */
    }
}

void Interloper::compile(const std::vector<std::string> &lines)
{

    const auto tokens = lexer.tokenize(&lines);

    if(lexer.error)
    {
        exit(1);
    }

    //print_tokens(tokens);
    parser.init(&lines,&tokens);
    parser.parse(&root);
    assert(root);
    parser.print(root);

    if(parser.error)
    {
        exit(1);
    }



    // okay now we need to start doing semantic analysis
    // first handle any imports, macros etc (skip for now)
    // handle any type declartions (skip for now)
    // go through every function and get all the variable declarations
    // okay now just run through and emit a 3 operand code (which operates on vars)
    // finally translate to actual bytecode with registers stack accesses etc



    parse_function_declarations();

}

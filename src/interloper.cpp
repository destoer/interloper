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
    parser.print(root);

    if(parser.error)
    {
        exit(1);
    }
}

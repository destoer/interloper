

using INSTR_FUNC = void(*)(Function& func);
using PARSER_FUNC = void(*)(Parser& parser);

// for the builtin in IR
// we will just reuse the existing table
struct InstrHandler
{
    op_type type;
    PARSER_FUNC parser_fptr;
    INSTR_FUNC instr_fptr;
};

// NOTE: this is not intended to assemble the standard set of directives
// we really only want this for doing fiddly things like messing with control registers
// not writing full blown functions for now
std::map<std::string,InstrHandler> instr_table =
{

};

AstNode* asm_block(Parser& parser)
{
    AstNode* block = ast_plain(ast_type::block);

    consume(parser,token_type::left_c_brace);

    while(!match(parser,token_type::right_c_brace))
    {
        const auto t = next_token(parser);

        if(t.type == token_type::eof)
        {
            panic(parser,t,"unterminated asm block");
            return nullptr;
        }

        if(!instr_table.count(t.literal))
        {
            panic(parser,t,"unknown instruction %s\n",t.literal.c_str());
            return nullptr;
        }

        const auto handler = instr_table[t.literal];

        const auto info = OPCODE_TABLE[u32(handler.type)];

        switch(info.group)
        {

            case op_group::reg_t: unimplemented("asm reg");
            case op_group::regm_t: unimplemented("asm regm");
            case op_group::imm_t: unimplemented("asm imm");
            case op_group::load_t: unimplemented("asm load");
            case op_group::store_t: unimplemented("asm store");
            case op_group::implicit_t: unimplemented("asm implicit");
            case op_group::branch_t: unimplemented("asm branch");
            case op_group::slot_t: unimplemented("asm slot");
                        
        }

    }

    consume(parser,token_type::right_c_brace);

    return block;
}
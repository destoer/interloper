static constexpr u32 KEYWORD_TABLE_SIZE = 128;

static constexpr HashNode<String,token_type> KEYWORD_TABLE[KEYWORD_TABLE_SIZE] = 
{
    {"",token_type::error},
    {"",token_type::error},
    {"struct",token_type::struct_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"u16",token_type::u16},
    {"",token_type::error},
    {"",token_type::error},
    {"else",token_type::else_t},
    {"",token_type::error},
    {"false",token_type::false_t},
    {"",token_type::error},
    {"",token_type::error},
    {"bool",token_type::bool_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"func",token_type::func},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"return",token_type::ret},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"type_alias",token_type::type_alias},
    {"",token_type::error},
    {"",token_type::error},
    {"s32",token_type::s32},
    {"",token_type::error},
    {"u32",token_type::u32},
    {"s8",token_type::s8},
    {"default",token_type::default_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"import",token_type::import},
    {"",token_type::error},
    {"if",token_type::if_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"switch",token_type::switch_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"u8",token_type::u8},
    {"",token_type::error},
    {"while",token_type::while_t},
    {"NULL",token_type::null_t},
    {"",token_type::error},
    {"s16",token_type::s16},
    {"",token_type::error},
    {"",token_type::error},
    {"const",token_type::const_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"true",token_type::true_t},
    {"",token_type::error},
    {"",token_type::error},
    {"c8",token_type::c8_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"sizeof",token_type::sizeof_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"for",token_type::for_t},
    {"enum",token_type::enum_t},
    {"cast",token_type::cast},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"decl",token_type::decl},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"case",token_type::case_t},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"",token_type::error},
    {"byte",token_type::byte_t},
};
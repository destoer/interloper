#include <lib.cpp>
#include <lexer.h>
#include <token.h>


const char* KEYWORD[][2] =
{

    {tok_name(token_type::for_t),"token_type::for_t"},
    {tok_name(token_type::if_t),"token_type::if_t"},
    {tok_name(token_type::else_t),"token_type::else_t"},

    {tok_name(token_type::decl),"token_type::decl"},
    {tok_name(token_type::const_t),"token_type::const_t"},

    {tok_name(token_type::u8),"token_type::u8"},
    {tok_name(token_type::u16),"token_type::u16"},
    {tok_name(token_type::u32),"token_type::u32"},

    {tok_name(token_type::s8),"token_type::s8"},
    {tok_name(token_type::s16),"token_type::s16"},
    {tok_name(token_type::s32),"token_type::s32"},
    {tok_name(token_type::byte_t),"token_type::byte_t"},
    {tok_name(token_type::bool_t),"token_type::bool_t"},

    {tok_name(token_type::false_t),"token_type::false_t"},
    {tok_name(token_type::true_t),"token_type::true_t"},
    {tok_name(token_type::null_t),"token_type::null_t"},

    {tok_name(token_type::import),"token_type::import"},
    {tok_name(token_type::struct_t),"token_type::struct_t"},

    {tok_name(token_type::cast),"token_type::cast"},
    {tok_name(token_type::sizeof_t),"token_type::sizeof_t"},
    {tok_name(token_type::func),"token_type::func"},
    {tok_name(token_type::ret),"token_type::ret"},
};

const u32 KEYWORD_SIZE = sizeof(KEYWORD) / sizeof(KEYWORD[0]);


const char* INTRIN[][2]  = 
{
    {"intrin_syscall","&intrin_syscall"},
};


const u32 INTRIN_SIZE = sizeof(INTRIN) / sizeof(INTRIN[0]);


void gen_table(const char* key[][2],const char* name, const char* type, const char* error, u32 key_size)
{
    const u32 table_size = bit_ceil(key_size) * 2;

    static constexpr u32 SLOT_EMPTY = 0xffffffff;

    printf("static constexpr u32 %s_TABLE_SIZE = %d;\n",name,table_size);

    u32 hash_table[table_size];


    for(u32 i = 0; i < table_size; i++)
    {
        hash_table[i] = SLOT_EMPTY;
    }

    u32 collision = 0;

    for(u32 i = 0; i < key_size; i++)
    {
        u32 slot = hash_slot(table_size,key[i][0]);

        if(hash_table[slot] == SLOT_EMPTY)
        {
            hash_table[slot] = i;
        }

        // slot has a collision find another for it
        else
        {
            collision++;

            while(hash_table[slot] != SLOT_EMPTY)
            {
                slot = (slot + 1) & (table_size - 1);
            }

            hash_table[slot] = i;
        }
    }

    printf("collisions: %d\n",collision);

    printf("static constexpr HashNode<%s> %s_TABLE[%s_TABLE_SIZE] = \n{\n",type,name,name);

    for(u32 i = 0; i < table_size; i++)
    {
        if(hash_table[i] == SLOT_EMPTY)
        {
            printf("    {\"\",%s},\n",error);
        }

        else
        {
            printf("    {\"%s\",%s},\n",key[hash_table[i]][0], key[hash_table[i]][1]);
        }
    }

    printf("};\n");    
}

int main()
{
    gen_table(KEYWORD,"KEYWORD","token_type","token_type::error",KEYWORD_SIZE);
    gen_table(INTRIN,"INTRIN","INTRIN_FUNC","nullptr",INTRIN_SIZE);
}
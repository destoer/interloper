#include <destoer.h>
using namespace destoer;

#include <lexer_lut.cpp>

void dump_state_table()
{
	printf("{\n");
	for(u32 i = 0; i < LEX_CLASS_SIZE; i++)
	{
		printf("\tLEX_CHAR_ERROR, //%s\n",LEX_CLASS_NAMES[i]);
	}
	printf("},\n");
}

void dump_class_table()
{
	const char* default_state = "lex_class::error";

    printf("lex_class LEX_CLASS[256] = {\n");

	for(int i = 0; i < 256; i++)
	{
        printf("\t");
		if(isalpha(i))
		{
			printf("lex_class::alpha, //'%c'\n",i);
		}

		else if(isdigit(i))
		{
			printf("lex_class::digit, //'%c'\n",i);
		}

		else if(isprint(i))
		{
			printf("%s, //'%c'(%x)\n",default_state,i,i);
		}

		else if(i == '\n')
		{
			printf("%s, // linefeed\n",default_state);
		}		

		else if(i == '\t')
		{
			printf("%s, // tab\n",default_state);
		}		

		
		else if(i == '\r')
		{
			printf("%s, // carrige\n",default_state);
		}		


		else
		{
			printf("%s, //(%x)\n",default_state,i);
		}
	}

    printf("};\n");	
}

void dump_misc_table()
{
    printf("token_type LEX_CLASS[128] = {\n");

	for(int i = 0; i < 128; i++)
	{
        printf("\t");
		if(isprint(i))
		{
			printf("token_type::error, //'%c'\n",i);
		}

		else
		{
			printf("token_type::error, //(%x)\n",i);
		}
	}

    printf("};\n");	
}

int main()
{
	dump_misc_table();

	//dump_class_table();

	//dump_state_table();
}

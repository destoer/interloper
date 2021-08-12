#include <interloper.h>


template<typename F>
bool verify_immediate_internal(const std::string &line, uint32_t &i, F lambda)
{
    const auto len = line.size();

    for(; i < len; i++)
    {
        // valid part of the value
        if(lambda(line[i]))
        {
            continue;
        }

        // values cannot have these at the end!
        else if(isalpha(line[i]))
        {
            return false;
        }

        // we have  < ; + , etc stop parsing
        else 
        {
            return true;
        }
    }

    return true;
}


bool verify_immediate(const std::string &line, std::string &literal)
{
    const auto len = line.size();

    // an empty immediate aint much use to us
    if(!len)
    {
        return false;
    }

    uint32_t i = 0;

    const auto c = line[0];

    // allow - or +
    if(c == '-' || c == '+')
    {
        i = 1;
        // no digit after the sign is of no use
        if(len == 1)
        {
            return false;
        }
    }

    bool valid = false;


    // have prefix + one more digit at minimum
    const auto prefix = i+2 < len?  line.substr(i,2) : "";

    // verify we have a valid hex number
    if(prefix == "0x")
    {
        // skip past the prefix
        i += 2;
        valid = verify_immediate_internal(line,i,[](const char c) 
        {
            return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
        });
    }

    // verify its ones or zeros
    else if(prefix == "0b")
    {
        // skip past the prefix
        i += 2;                
        valid = verify_immediate_internal(line,i,[](const char c) 
        {
            return c == '0' || c == '1';
        });
    }

    // verify we have all digits
    else
    {
        valid = verify_immediate_internal(line,i,[](const char c) 
        {
            return c >= '0' && c <= '9';
        });
    }
    

    if(valid)
    {
        literal = line.substr(0,i);
    }

    return valid;    
}

// TODO: make sure the number fits in 32 bits
uint32_t convert_imm(const std::string &imm)
{
    if(imm.size() >= 3 && imm.substr(0,2) == "0b")
    {
        return static_cast<uint32_t>(std::stoi(imm.substr(2),0,2));
    }

    // stoi wont auto detect base for binary strings?
    return static_cast<uint32_t>(std::stoi(imm,0,0));
}

void Lexer::decode_imm(const std::string &line, uint32_t &i,std::vector<Token> &tokens)
{
    std::string literal = "";

    const auto success = verify_immediate(line.substr(i),literal);

    if(!success)
    {
        panic("invalid immediate: %s\n",line.c_str());
    }

    // set one back for whatever the terminating character was
    i--;

    i += literal.size();

    insert_token(tokens,token_type::value,literal);    
}
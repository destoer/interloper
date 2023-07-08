struct TokenChain
{
    char c;
    token_type type;
};

struct LexerLookup
{
    token_type type = token_type::error;
    const TokenChain* chain = nullptr;
    u32 chain_size = 0;
};

static constexpr u32 EQUAL_CHAIN_SIZE = 1;
const TokenChain EQUAL_CHAIN[EQUAL_CHAIN_SIZE] = 
{
    {'=',token_type::logical_eq},
};

static constexpr u32 COLON_CHAIN_SIZE = 2;
const TokenChain COLON_CHAIN[COLON_CHAIN_SIZE] = 
{
    {'=',token_type::decl},
    {':',token_type::scope},
};

static constexpr u32 TIMES_CHAIN_SIZE = 1;
const TokenChain TIMES_CHAIN[TIMES_CHAIN_SIZE] = 
{
    {'=',token_type::times_eq},
};

static constexpr u32 PLUS_CHAIN_SIZE = 2;
const TokenChain PLUS_CHAIN[PLUS_CHAIN_SIZE] = 
{
    {'=',token_type::plus_eq},
    {'+',token_type::increment},
};

static constexpr u32 OR_CHAIN_SIZE = 1;
const TokenChain OR_CHAIN[OR_CHAIN_SIZE] = 
{
    {'|',token_type::logical_or},
};

static constexpr u32 AND_CHAIN_SIZE = 1;
const TokenChain AND_CHAIN[AND_CHAIN_SIZE] = 
{
    {'&',token_type::logical_and},
};

static constexpr u32 NOT_CHAIN_SIZE = 1;
const TokenChain NOT_CHAIN[NOT_CHAIN_SIZE] = 
{
    {'=',token_type::logical_ne},
};

static constexpr u32 GT_CHAIN_SIZE = 2;
const TokenChain GT_CHAIN[GT_CHAIN_SIZE] = 
{
    {'=',token_type::logical_ge},
    {'>',token_type::shift_r},
};

static constexpr u32 LT_CHAIN_SIZE = 2;
const TokenChain LT_CHAIN[LT_CHAIN_SIZE] = 
{
    {'=',token_type::logical_le},
    {'<',token_type::shift_l},
};

const LexerLookup LEXER_LOOKUP[256] = 
{
    {token_type::error}, //(0)
    {token_type::error}, //(1)
    {token_type::error}, //(2)
    {token_type::error}, //(3)
    {token_type::error}, //(4)
    {token_type::error}, //(5)
    {token_type::error}, //(6)
    {token_type::error}, //(7)
    {token_type::error}, //(8)
    {token_type::error}, //'\t'(9)
    {token_type::error}, //'\n'(a)
    {token_type::error}, //(b)
    {token_type::error}, //(c)
    {token_type::error}, //(d)
    {token_type::error}, //(e)
    {token_type::error}, //(f)
    {token_type::error}, //(10)
    {token_type::error}, //(11)
    {token_type::error}, //(12)
    {token_type::error}, //(13)
    {token_type::error}, //(14)
    {token_type::error}, //(15)
    {token_type::error}, //(16)
    {token_type::error}, //(17)
    {token_type::error}, //(18)
    {token_type::error}, //(19)
    {token_type::error}, //(1a)
    {token_type::error}, //(1b)
    {token_type::error}, //(1c)
    {token_type::error}, //(1d)
    {token_type::error}, //(1e)
    {token_type::error}, //(1f)
    {token_type::error}, //' '(20)
    {token_type::logical_not,NOT_CHAIN,NOT_CHAIN_SIZE}, //'!'(21)
    {token_type::error}, //'"'(22)
    {token_type::error}, //'#'(23)
    {token_type::error}, //'$'(24)
    {token_type::mod}, //'%'(25)
    {token_type::operator_and,AND_CHAIN,AND_CHAIN_SIZE}, //'&'(26)
    {token_type::error}, //'''(27)
    {token_type::left_paren}, //'('(28)
    {token_type::right_paren}, //')'(29)
    {token_type::times,TIMES_CHAIN,TIMES_CHAIN_SIZE}, //'*'(2a)
    {token_type::plus,PLUS_CHAIN,PLUS_CHAIN_SIZE}, //'+'(2b)
    {token_type::comma}, //','(2c)
    {token_type::error}, //'-'(2d)
    {token_type::error}, //'.'(2e)
    {token_type::error}, //'/'(2f)
    {token_type::error}, //'0'(30)
    {token_type::error}, //'1'(31)
    {token_type::error}, //'2'(32)
    {token_type::error}, //'3'(33)
    {token_type::error}, //'4'(34)
    {token_type::error}, //'5'(35)
    {token_type::error}, //'6'(36)
    {token_type::error}, //'7'(37)
    {token_type::error}, //'8'(38)
    {token_type::error}, //'9'(39)
    {token_type::colon,COLON_CHAIN,COLON_CHAIN_SIZE}, //':'(3a)
    {token_type::semi_colon}, //';'(3b)
    {token_type::logical_lt,LT_CHAIN,LT_CHAIN_SIZE}, //'<'(3c)
    {token_type::equal,EQUAL_CHAIN,EQUAL_CHAIN_SIZE}, //'='(3d)
    {token_type::logical_gt,GT_CHAIN,GT_CHAIN_SIZE}, //'>'(3e)
    {token_type::qmark}, //'?'(3f)
    {token_type::deref}, //'@'(40)
    {token_type::error}, //'A'(41)
    {token_type::error}, //'B'(42)
    {token_type::error}, //'C'(43)
    {token_type::error}, //'D'(44)
    {token_type::error}, //'E'(45)
    {token_type::error}, //'F'(46)
    {token_type::error}, //'G'(47)
    {token_type::error}, //'H'(48)
    {token_type::error}, //'I'(49)
    {token_type::error}, //'J'(4a)
    {token_type::error}, //'K'(4b)
    {token_type::error}, //'L'(4c)
    {token_type::error}, //'M'(4d)
    {token_type::error}, //'N'(4e)
    {token_type::error}, //'O'(4f)
    {token_type::error}, //'P'(50)
    {token_type::error}, //'Q'(51)
    {token_type::error}, //'R'(52)
    {token_type::error}, //'S'(53)
    {token_type::error}, //'T'(54)
    {token_type::error}, //'U'(55)
    {token_type::error}, //'V'(56)
    {token_type::error}, //'W'(57)
    {token_type::error}, //'X'(58)
    {token_type::error}, //'Y'(59)
    {token_type::error}, //'Z'(5a)
    {token_type::sl_brace}, //'['(5b)
    {token_type::error}, //'\'(5c)
    {token_type::sr_brace}, //']'(5d)
    {token_type::bitwise_xor}, //'^'(5e)
    {token_type::error}, //'_'(5f)
    {token_type::error}, //'`'(60)
    {token_type::error}, //'a'(61)
    {token_type::error}, //'b'(62)
    {token_type::error}, //'c'(63)
    {token_type::error}, //'d'(64)
    {token_type::error}, //'e'(65)
    {token_type::error}, //'f'(66)
    {token_type::error}, //'g'(67)
    {token_type::error}, //'h'(68)
    {token_type::error}, //'i'(69)
    {token_type::error}, //'j'(6a)
    {token_type::error}, //'k'(6b)
    {token_type::error}, //'l'(6c)
    {token_type::error}, //'m'(6d)
    {token_type::error}, //'n'(6e)
    {token_type::error}, //'o'(6f)
    {token_type::error}, //'p'(70)
    {token_type::error}, //'q'(71)
    {token_type::error}, //'r'(72)
    {token_type::error}, //'s'(73)
    {token_type::error}, //'t'(74)
    {token_type::error}, //'u'(75)
    {token_type::error}, //'v'(76)
    {token_type::error}, //'w'(77)
    {token_type::error}, //'x'(78)
    {token_type::error}, //'y'(79)
    {token_type::error}, //'z'(7a)
    {token_type::left_c_brace}, //'{'(7b)
    {token_type::bitwise_or,OR_CHAIN,OR_CHAIN_SIZE}, //'|'(7c)
    {token_type::right_c_brace}, //'}'(7d)
    {token_type::bitwise_not}, //'~'(7e)
    {token_type::error}, //(7f)
    {token_type::error}, //(80)
    {token_type::error}, //(81)
    {token_type::error}, //(82)
    {token_type::error}, //(83)
    {token_type::error}, //(84)
    {token_type::error}, //(85)
    {token_type::error}, //(86)
    {token_type::error}, //(87)
    {token_type::error}, //(88)
    {token_type::error}, //(89)
    {token_type::error}, //(8a)
    {token_type::error}, //(8b)
    {token_type::error}, //(8c)
    {token_type::error}, //(8d)
    {token_type::error}, //(8e)
    {token_type::error}, //(8f)
    {token_type::error}, //(90)
    {token_type::error}, //(91)
    {token_type::error}, //(92)
    {token_type::error}, //(93)
    {token_type::error}, //(94)
    {token_type::error}, //(95)
    {token_type::error}, //(96)
    {token_type::error}, //(97)
    {token_type::error}, //(98)
    {token_type::error}, //(99)
    {token_type::error}, //(9a)
    {token_type::error}, //(9b)
    {token_type::error}, //(9c)
    {token_type::error}, //(9d)
    {token_type::error}, //(9e)
    {token_type::error}, //(9f)
    {token_type::error}, //(a0)
    {token_type::error}, //(a1)
    {token_type::error}, //(a2)
    {token_type::error}, //(a3)
    {token_type::error}, //(a4)
    {token_type::error}, //(a5)
    {token_type::error}, //(a6)
    {token_type::error}, //(a7)
    {token_type::error}, //(a8)
    {token_type::error}, //(a9)
    {token_type::error}, //(aa)
    {token_type::error}, //(ab)
    {token_type::error}, //(ac)
    {token_type::error}, //(ad)
    {token_type::error}, //(ae)
    {token_type::error}, //(af)
    {token_type::error}, //(b0)
    {token_type::error}, //(b1)
    {token_type::error}, //(b2)
    {token_type::error}, //(b3)
    {token_type::error}, //(b4)
    {token_type::error}, //(b5)
    {token_type::error}, //(b6)
    {token_type::error}, //(b7)
    {token_type::error}, //(b8)
    {token_type::error}, //(b9)
    {token_type::error}, //(ba)
    {token_type::error}, //(bb)
    {token_type::error}, //(bc)
    {token_type::error}, //(bd)
    {token_type::error}, //(be)
    {token_type::error}, //(bf)
    {token_type::error}, //(c0)
    {token_type::error}, //(c1)
    {token_type::error}, //(c2)
    {token_type::error}, //(c3)
    {token_type::error}, //(c4)
    {token_type::error}, //(c5)
    {token_type::error}, //(c6)
    {token_type::error}, //(c7)
    {token_type::error}, //(c8)
    {token_type::error}, //(c9)
    {token_type::error}, //(ca)
    {token_type::error}, //(cb)
    {token_type::error}, //(cc)
    {token_type::error}, //(cd)
    {token_type::error}, //(ce)
    {token_type::error}, //(cf)
    {token_type::error}, //(d0)
    {token_type::error}, //(d1)
    {token_type::error}, //(d2)
    {token_type::error}, //(d3)
    {token_type::error}, //(d4)
    {token_type::error}, //(d5)
    {token_type::error}, //(d6)
    {token_type::error}, //(d7)
    {token_type::error}, //(d8)
    {token_type::error}, //(d9)
    {token_type::error}, //(da)
    {token_type::error}, //(db)
    {token_type::error}, //(dc)
    {token_type::error}, //(dd)
    {token_type::error}, //(de)
    {token_type::error}, //(df)
    {token_type::error}, //(e0)
    {token_type::error}, //(e1)
    {token_type::error}, //(e2)
    {token_type::error}, //(e3)
    {token_type::error}, //(e4)
    {token_type::error}, //(e5)
    {token_type::error}, //(e6)
    {token_type::error}, //(e7)
    {token_type::error}, //(e8)
    {token_type::error}, //(e9)
    {token_type::error}, //(ea)
    {token_type::error}, //(eb)
    {token_type::error}, //(ec)
    {token_type::error}, //(ed)
    {token_type::error}, //(ee)
    {token_type::error}, //(ef)
    {token_type::error}, //(f0)
    {token_type::error}, //(f1)
    {token_type::error}, //(f2)
    {token_type::error}, //(f3)
    {token_type::error}, //(f4)
    {token_type::error}, //(f5)
    {token_type::error}, //(f6)
    {token_type::error}, //(f7)
    {token_type::error}, //(f8)
    {token_type::error}, //(f9)
    {token_type::error}, //(fa)
    {token_type::error}, //(fb)
    {token_type::error}, //(fc)
    {token_type::error}, //(fd)
    {token_type::error}, //(fe)
    {token_type::error}, //(ff)
};
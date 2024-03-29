#include <token.h>

static constexpr u32  LEX_STATE_LOWER_IDX = TOKEN_SIZE + 0;

// termination
static constexpr u32 LEX_STATE_END = LEX_STATE_LOWER_IDX + 0;
static constexpr u32 LEX_STATE_INVALID_CHAR = LEX_STATE_END + 0;

static constexpr u32 LEX_STATE_SYM_FIN = LEX_STATE_END + 1; 
static constexpr u32 LEX_STATE_INT_FIN = LEX_STATE_END + 2;
static constexpr u32 LEX_STATE_STRING_FIN = LEX_STATE_END + 3;
static constexpr u32 LEX_STATE_MISC_FIN = LEX_STATE_END + 4;

// terminator operators
static constexpr u32 LEX_STATE_EOF = LEX_STATE_END + 6;
static constexpr u32 LEX_STATE_CHAR = LEX_STATE_END + 7;
static constexpr u32 LEX_STATE_DOT = LEX_STATE_END + 8;
static constexpr u32 LEX_STATE_FORWARD_SLASH = LEX_STATE_END + 9;

// active
// NOTE: keep this immedialty following termination 
static constexpr u32 LEX_STATE_ACTIVE = LEX_STATE_FORWARD_SLASH + 1;

static constexpr u32 LEX_STATE_START = LEX_STATE_ACTIVE + 0;
static constexpr u32 LEX_STATE_SYM = LEX_STATE_ACTIVE + 1; 
static constexpr u32 LEX_STATE_TIMES = LEX_STATE_ACTIVE + 2;
static constexpr u32 LEX_STATE_LT = LEX_STATE_ACTIVE + 3;  
static constexpr u32 LEX_STATE_GT = LEX_STATE_ACTIVE + 4;
static constexpr u32 LEX_STATE_COLON = LEX_STATE_ACTIVE + 5;  
static constexpr u32 LEX_STATE_EQ = LEX_STATE_ACTIVE + 6;
static constexpr u32 LEX_STATE_OR = LEX_STATE_ACTIVE + 7;
static constexpr u32 LEX_STATE_AND = LEX_STATE_ACTIVE + 8;
static constexpr u32 LEX_STATE_NOT = LEX_STATE_ACTIVE + 9;
static constexpr u32 LEX_STATE_XOR = LEX_STATE_ACTIVE + 10;
static constexpr u32 LEX_STATE_MOD = LEX_STATE_ACTIVE + 11;
static constexpr u32 LEX_STATE_MINUS = LEX_STATE_ACTIVE + 12;
static constexpr u32 LEX_STATE_PLUS = LEX_STATE_ACTIVE + 13;

static constexpr u32 LEX_ACTIVE_STATE_SIZE = (LEX_STATE_PLUS + 1) - LEX_STATE_ACTIVE; 

const char* LEXER_ACTIVE_STATE_NAMES[LEX_ACTIVE_STATE_SIZE] = 
{
    "start",
    "sym",
	"times",
	"lt",
	"gt",
	"colon",
	"equal",
	"or",
	"and",
	"not",
	"xor",
	"mod",
	"sub",
	"add",
};

enum class lex_class
{
    whitespace,
    digit,
    alpha,
    token_misc,
    single_quote,
    double_quote,
    forward_slash,
    plus,
    minus,
    times,
    and_t,
    or_t,
    xor_t,
    gt,
    lt,
    colon,
    equal,
	not_t,
	dot,
	mod,
    eof,
    error,
};

static constexpr u32 LEX_CLASS_SIZE = u32(lex_class::error) + 1;

const char* LEX_CLASS_NAMES[LEX_CLASS_SIZE] = 
{
    "whitespace",
    "digit",
    "alpha",
    "token_misc",
    "single_quote",
    "double_quote",
    "forward_slash",
    "plus",
    "minus",
    "times",
    "and",
    "or",
    "xor",
    "gt",
    "lt",
    "colon",
    "equal",
	"not",
	"dot",
	"mod",
    "eof",
    "error",
};

lex_class LEX_CLASS[256] = {
	lex_class::eof, //(0)
	lex_class::error, //(1)
	lex_class::error, //(2)
	lex_class::error, //(3)
	lex_class::error, //(4)
	lex_class::error, //(5)
	lex_class::error, //(6)
	lex_class::error, //(7)
	lex_class::error, //(8)
	lex_class::whitespace, // tab
	lex_class::whitespace, // linefeed
	lex_class::error, //(b)
	lex_class::error, //(c)
	lex_class::whitespace, // carrige
	lex_class::error, //(e)
	lex_class::error, //(f)
	lex_class::error, //(10)
	lex_class::error, //(11)
	lex_class::error, //(12)
	lex_class::error, //(13)
	lex_class::error, //(14)
	lex_class::error, //(15)
	lex_class::error, //(16)
	lex_class::error, //(17)
	lex_class::error, //(18)
	lex_class::error, //(19)
	lex_class::error, //(1a)
	lex_class::error, //(1b)
	lex_class::error, //(1c)
	lex_class::error, //(1d)
	lex_class::error, //(1e)
	lex_class::error, //(1f)
	lex_class::whitespace, //' '(20)
	lex_class::not_t, //'!'(21)
	lex_class::double_quote, //'"'(22)
	lex_class::token_misc, //'#'(23)
	lex_class::error, //'$'(24)
	lex_class::mod, //'%'(25)
	lex_class::and_t, //'&'(26)
	lex_class::single_quote, //'''(27)
	lex_class::token_misc, //'('(28)
	lex_class::token_misc, //')'(29)
	lex_class::times, //'*'(2a)
	lex_class::plus, //'+'(2b)
	lex_class::token_misc, //','(2c)
	lex_class::minus, //'-'(2d)
	lex_class::dot, //'.'(2e)
	lex_class::forward_slash, //'/'(2f)
	lex_class::digit, //'0'
	lex_class::digit, //'1'
	lex_class::digit, //'2'
	lex_class::digit, //'3'
	lex_class::digit, //'4'
	lex_class::digit, //'5'
	lex_class::digit, //'6'
	lex_class::digit, //'7'
	lex_class::digit, //'8'
	lex_class::digit, //'9'
	lex_class::colon, //':'(3a)
	lex_class::token_misc, //';'(3b)
	lex_class::lt, //'<'(3c)
	lex_class::equal, //'='(3d)
	lex_class::gt, //'>'(3e)
	lex_class::token_misc, //'?'(3f)
	lex_class::token_misc, //'@'(40)
	lex_class::alpha, //'A'
	lex_class::alpha, //'B'
	lex_class::alpha, //'C'
	lex_class::alpha, //'D'
	lex_class::alpha, //'E'
	lex_class::alpha, //'F'
	lex_class::alpha, //'G'
	lex_class::alpha, //'H'
	lex_class::alpha, //'I'
	lex_class::alpha, //'J'
	lex_class::alpha, //'K'
	lex_class::alpha, //'L'
	lex_class::alpha, //'M'
	lex_class::alpha, //'N'
	lex_class::alpha, //'O'
	lex_class::alpha, //'P'
	lex_class::alpha, //'Q'
	lex_class::alpha, //'R'
	lex_class::alpha, //'S'
	lex_class::alpha, //'T'
	lex_class::alpha, //'U'
	lex_class::alpha, //'V'
	lex_class::alpha, //'W'
	lex_class::alpha, //'X'
	lex_class::alpha, //'Y'
	lex_class::alpha, //'Z'
	lex_class::token_misc, //'['(5b)
	lex_class::error, //'\'(5c)
	lex_class::token_misc, //']'(5d)
	lex_class::xor_t, //'^'(5e)
	lex_class::alpha, //'_'(5f)
	lex_class::error, //'`'(60)
	lex_class::alpha, //'a'
	lex_class::alpha, //'b'
	lex_class::alpha, //'c'
	lex_class::alpha, //'d'
	lex_class::alpha, //'e'
	lex_class::alpha, //'f'
	lex_class::alpha, //'g'
	lex_class::alpha, //'h'
	lex_class::alpha, //'i'
	lex_class::alpha, //'j'
	lex_class::alpha, //'k'
	lex_class::alpha, //'l'
	lex_class::alpha, //'m'
	lex_class::alpha, //'n'
	lex_class::alpha, //'o'
	lex_class::alpha, //'p'
	lex_class::alpha, //'q'
	lex_class::alpha, //'r'
	lex_class::alpha, //'s'
	lex_class::alpha, //'t'
	lex_class::alpha, //'u'
	lex_class::alpha, //'v'
	lex_class::alpha, //'w'
	lex_class::alpha, //'x'
	lex_class::alpha, //'y'
	lex_class::alpha, //'z'
	lex_class::token_misc, //'{'(7b)
	lex_class::or_t, //'|'(7c)
	lex_class::token_misc, //'}'(7d)
	lex_class::token_misc, //'~'(7e)
	lex_class::error, //(7f)
	lex_class::error, //(80)
	lex_class::error, //(81)
	lex_class::error, //(82)
	lex_class::error, //(83)
	lex_class::error, //(84)
	lex_class::error, //(85)
	lex_class::error, //(86)
	lex_class::error, //(87)
	lex_class::error, //(88)
	lex_class::error, //(89)
	lex_class::error, //(8a)
	lex_class::error, //(8b)
	lex_class::error, //(8c)
	lex_class::error, //(8d)
	lex_class::error, //(8e)
	lex_class::error, //(8f)
	lex_class::error, //(90)
	lex_class::error, //(91)
	lex_class::error, //(92)
	lex_class::error, //(93)
	lex_class::error, //(94)
	lex_class::error, //(95)
	lex_class::error, //(96)
	lex_class::error, //(97)
	lex_class::error, //(98)
	lex_class::error, //(99)
	lex_class::error, //(9a)
	lex_class::error, //(9b)
	lex_class::error, //(9c)
	lex_class::error, //(9d)
	lex_class::error, //(9e)
	lex_class::error, //(9f)
	lex_class::error, //(a0)
	lex_class::error, //(a1)
	lex_class::error, //(a2)
	lex_class::error, //(a3)
	lex_class::error, //(a4)
	lex_class::error, //(a5)
	lex_class::error, //(a6)
	lex_class::error, //(a7)
	lex_class::error, //(a8)
	lex_class::error, //(a9)
	lex_class::error, //(aa)
	lex_class::error, //(ab)
	lex_class::error, //(ac)
	lex_class::error, //(ad)
	lex_class::error, //(ae)
	lex_class::error, //(af)
	lex_class::error, //(b0)
	lex_class::error, //(b1)
	lex_class::error, //(b2)
	lex_class::error, //(b3)
	lex_class::error, //(b4)
	lex_class::error, //(b5)
	lex_class::error, //(b6)
	lex_class::error, //(b7)
	lex_class::error, //(b8)
	lex_class::error, //(b9)
	lex_class::error, //(ba)
	lex_class::error, //(bb)
	lex_class::error, //(bc)
	lex_class::error, //(bd)
	lex_class::error, //(be)
	lex_class::error, //(bf)
	lex_class::error, //(c0)
	lex_class::error, //(c1)
	lex_class::error, //(c2)
	lex_class::error, //(c3)
	lex_class::error, //(c4)
	lex_class::error, //(c5)
	lex_class::error, //(c6)
	lex_class::error, //(c7)
	lex_class::error, //(c8)
	lex_class::error, //(c9)
	lex_class::error, //(ca)
	lex_class::error, //(cb)
	lex_class::error, //(cc)
	lex_class::error, //(cd)
	lex_class::error, //(ce)
	lex_class::error, //(cf)
	lex_class::error, //(d0)
	lex_class::error, //(d1)
	lex_class::error, //(d2)
	lex_class::error, //(d3)
	lex_class::error, //(d4)
	lex_class::error, //(d5)
	lex_class::error, //(d6)
	lex_class::error, //(d7)
	lex_class::error, //(d8)
	lex_class::error, //(d9)
	lex_class::error, //(da)
	lex_class::error, //(db)
	lex_class::error, //(dc)
	lex_class::error, //(dd)
	lex_class::error, //(de)
	lex_class::error, //(df)
	lex_class::error, //(e0)
	lex_class::error, //(e1)
	lex_class::error, //(e2)
	lex_class::error, //(e3)
	lex_class::error, //(e4)
	lex_class::error, //(e5)
	lex_class::error, //(e6)
	lex_class::error, //(e7)
	lex_class::error, //(e8)
	lex_class::error, //(e9)
	lex_class::error, //(ea)
	lex_class::error, //(eb)
	lex_class::error, //(ec)
	lex_class::error, //(ed)
	lex_class::error, //(ee)
	lex_class::error, //(ef)
	lex_class::error, //(f0)
	lex_class::error, //(f1)
	lex_class::error, //(f2)
	lex_class::error, //(f3)
	lex_class::error, //(f4)
	lex_class::error, //(f5)
	lex_class::error, //(f6)
	lex_class::error, //(f7)
	lex_class::error, //(f8)
	lex_class::error, //(f9)
	lex_class::error, //(fa)
	lex_class::error, //(fb)
	lex_class::error, //(fc)
	lex_class::error, //(fd)
	lex_class::error, //(fe)
	lex_class::error, //(ff)
};


token_type LEX_TYPE[128] = {
	token_type::error, //(0)
	token_type::error, //(1)
	token_type::error, //(2)
	token_type::error, //(3)
	token_type::error, //(4)
	token_type::error, //(5)
	token_type::error, //(6)
	token_type::error, //(7)
	token_type::error, //(8)
	token_type::error, //(9)
	token_type::error, //(a)
	token_type::error, //(b)
	token_type::error, //(c)
	token_type::error, //(d)
	token_type::error, //(e)
	token_type::error, //(f)
	token_type::error, //(10)
	token_type::error, //(11)
	token_type::error, //(12)
	token_type::error, //(13)
	token_type::error, //(14)
	token_type::error, //(15)
	token_type::error, //(16)
	token_type::error, //(17)
	token_type::error, //(18)
	token_type::error, //(19)
	token_type::error, //(1a)
	token_type::error, //(1b)
	token_type::error, //(1c)
	token_type::error, //(1d)
	token_type::error, //(1e)
	token_type::error, //(1f)
	token_type::error, //' '
	token_type::error, //'!'
	token_type::error, //'"'
	token_type::hash, //'#'
	token_type::error, //'$'
	token_type::error, //'%'
	token_type::error, //'&'
	token_type::error, //'''
	token_type::left_paren, //'('
	token_type::right_paren, //')'
	token_type::error, //'*'
	token_type::error, //'+'
	token_type::comma, //','
	token_type::error, //'-'
	token_type::error, //'.'
	token_type::error, //'/'
	token_type::error, //'0'
	token_type::error, //'1'
	token_type::error, //'2'
	token_type::error, //'3'
	token_type::error, //'4'
	token_type::error, //'5'
	token_type::error, //'6'
	token_type::error, //'7'
	token_type::error, //'8'
	token_type::error, //'9'
	token_type::error, //':'
	token_type::semi_colon, //';'
	token_type::error, //'<'
	token_type::error, //'='
	token_type::error, //'>'
	token_type::qmark, //'?'
	token_type::deref, //'@'
	token_type::error, //'A'
	token_type::error, //'B'
	token_type::error, //'C'
	token_type::error, //'D'
	token_type::error, //'E'
	token_type::error, //'F'
	token_type::error, //'G'
	token_type::error, //'H'
	token_type::error, //'I'
	token_type::error, //'J'
	token_type::error, //'K'
	token_type::error, //'L'
	token_type::error, //'M'
	token_type::error, //'N'
	token_type::error, //'O'
	token_type::error, //'P'
	token_type::error, //'Q'
	token_type::error, //'R'
	token_type::error, //'S'
	token_type::error, //'T'
	token_type::error, //'U'
	token_type::error, //'V'
	token_type::error, //'W'
	token_type::error, //'X'
	token_type::error, //'Y'
	token_type::error, //'Z'
	token_type::sl_brace, //'['
	token_type::error, //'\'
	token_type::sr_brace, //']'
	token_type::error, //'^'
	token_type::error, //'_'
	token_type::error, //'`'
	token_type::error, //'a'
	token_type::error, //'b'
	token_type::error, //'c'
	token_type::error, //'d'
	token_type::error, //'e'
	token_type::error, //'f'
	token_type::error, //'g'
	token_type::error, //'h'
	token_type::error, //'i'
	token_type::error, //'j'
	token_type::error, //'k'
	token_type::error, //'l'
	token_type::error, //'m'
	token_type::error, //'n'
	token_type::error, //'o'
	token_type::error, //'p'
	token_type::error, //'q'
	token_type::error, //'r'
	token_type::error, //'s'
	token_type::error, //'t'
	token_type::error, //'u'
	token_type::error, //'v'
	token_type::error, //'w'
	token_type::error, //'x'
	token_type::error, //'y'
	token_type::error, //'z'
	token_type::left_c_brace, //'{'
	token_type::error, //'|'
	token_type::right_c_brace, //'}'
	token_type::bitwise_not, //'~'
	token_type::error, //(7f)
};


static constexpr u32 LEX_ACTIVE_STATES[LEX_ACTIVE_STATE_SIZE][LEX_CLASS_SIZE]
{
// start
{
	LEX_STATE_START, //whitespace
	LEX_STATE_INT_FIN, //digit
	LEX_STATE_SYM, //alpha
    LEX_STATE_MISC_FIN, //misc
	LEX_STATE_CHAR, //single_quote
	LEX_STATE_STRING_FIN, //double_quote
	LEX_STATE_FORWARD_SLASH, //forward_slash
	LEX_STATE_PLUS, //plus
	LEX_STATE_MINUS, //minus
	LEX_STATE_TIMES, //times
	LEX_STATE_AND, //and
	LEX_STATE_OR, //or
	LEX_STATE_XOR, //xor
	LEX_STATE_GT, //gt
	LEX_STATE_LT, //lt
	LEX_STATE_COLON, //colon
	LEX_STATE_EQ, //equal
	LEX_STATE_NOT, //not
	LEX_STATE_DOT, //dot
	LEX_STATE_MOD, // mod
	LEX_STATE_EOF, //eof
	LEX_STATE_INVALID_CHAR, //error
},

// sym
{
	LEX_STATE_SYM_FIN, //whitespace
	LEX_STATE_SYM, //digit
	LEX_STATE_SYM, //alpha
    LEX_STATE_SYM_FIN, //misc
	LEX_STATE_SYM_FIN, //single_quote
	LEX_STATE_SYM_FIN, //double_quote
	LEX_STATE_SYM_FIN, //forward_slash
	LEX_STATE_SYM_FIN, //plus
	LEX_STATE_SYM_FIN, //minus
	LEX_STATE_SYM_FIN, //times
	LEX_STATE_SYM_FIN, //and
	LEX_STATE_SYM_FIN, //or
	LEX_STATE_SYM_FIN, //xor
	LEX_STATE_SYM_FIN, //gt
	LEX_STATE_SYM_FIN, //lt
	LEX_STATE_SYM_FIN, //colon
	LEX_STATE_SYM_FIN, //equal
	LEX_STATE_SYM_FIN, //not
	LEX_STATE_SYM_FIN, //dot
	LEX_STATE_SYM_FIN, // mod
	LEX_STATE_SYM_FIN, //eof
	LEX_STATE_INVALID_CHAR, //error
},

// times
{
	u32(token_type::times), //whitespace
	u32(token_type::times), //digit
	u32(token_type::times), //alpha
    u32(token_type::times), //misc
	u32(token_type::times), //single_quote
	u32(token_type::times), //double_quote
	u32(token_type::times), //forward_slash
	u32(token_type::times), //plus
	u32(token_type::times), //minus
	u32(token_type::times), //times
	u32(token_type::times), //and
	u32(token_type::times), //or
	u32(token_type::times), //xor
	u32(token_type::times), //gt
	u32(token_type::times), //lt
	u32(token_type::times), //colon
	u32(token_type::times_eq), //equal
	u32(token_type::times), //not
	u32(token_type::times), //dot
	u32(token_type::times), // mod
	u32(token_type::times), //eof
	LEX_STATE_INVALID_CHAR, //error
},

// lt
{
	u32(token_type::logical_lt), //whitespace
	u32(token_type::logical_lt), //digit
	u32(token_type::logical_lt), //alpha
	u32(token_type::logical_lt), //token_misc
	u32(token_type::logical_lt), //single_quote
	u32(token_type::logical_lt), //double_quote
	u32(token_type::logical_lt), //forward_slash
	u32(token_type::logical_lt), //plus
	u32(token_type::logical_lt), //minus
	u32(token_type::logical_lt), //times
	u32(token_type::logical_lt), //and
	u32(token_type::logical_lt), //or
	u32(token_type::logical_lt), //xor
	u32(token_type::logical_lt), //gt
	u32(token_type::shift_l), //lt
	u32(token_type::logical_lt), //colon
	u32(token_type::logical_le), //equal
	u32(token_type::logical_lt), //not
	u32(token_type::logical_lt), //dot
	u32(token_type::logical_lt), //mod
	u32(token_type::logical_lt), //eof
	LEX_STATE_INVALID_CHAR,
},

//gt
{
	u32(token_type::logical_gt), //whitespace
	u32(token_type::logical_gt), //digit
	u32(token_type::logical_gt), //alpha
	u32(token_type::logical_gt), //token_misc
	u32(token_type::logical_gt), //single_quote
	u32(token_type::logical_gt), //double_quote
	u32(token_type::logical_gt), //forward_slash
	u32(token_type::logical_gt), //plus
	u32(token_type::logical_gt), //minus
	u32(token_type::logical_gt), //times
	u32(token_type::logical_gt), //and
	u32(token_type::logical_gt), //or
	u32(token_type::logical_gt), //xor
	u32(token_type::shift_r), //gt
	u32(token_type::logical_gt), //lt
	u32(token_type::logical_gt), //colon
	u32(token_type::logical_ge), //equal
	u32(token_type::logical_gt), //not
	u32(token_type::logical_gt), //dot
	u32(token_type::logical_gt), //mod
	u32(token_type::logical_gt), //eof
	LEX_STATE_INVALID_CHAR,
},


// colon
{
	u32(token_type::colon), //whitespace
	u32(token_type::colon), //digit
	u32(token_type::colon), //alpha
	u32(token_type::colon), //token_misc
	u32(token_type::colon), //single_quote
	u32(token_type::colon), //double_quote
	u32(token_type::colon), //forward_slash
	u32(token_type::colon), //plus
	u32(token_type::colon), //minus
	u32(token_type::colon), //times
	u32(token_type::colon), //and
	u32(token_type::colon), //or
	u32(token_type::colon), //xor
	u32(token_type::colon), //gt
	u32(token_type::colon), //lt
	u32(token_type::scope), //colon
	u32(token_type::decl), //equal
	u32(token_type::colon), //not
	u32(token_type::colon), //dot
	u32(token_type::colon), //mod
	u32(token_type::colon), //eof
	LEX_STATE_INVALID_CHAR,
},

// equal
{
	u32(token_type::equal), //whitespace
	u32(token_type::equal), //digit
	u32(token_type::equal), //alpha
	u32(token_type::equal), //token_misc
	u32(token_type::equal), //single_quote
	u32(token_type::equal), //double_quote
	u32(token_type::equal), //forward_slash
	u32(token_type::equal), //plus
	u32(token_type::equal), //minus
	u32(token_type::equal), //times
	u32(token_type::equal), //and
	u32(token_type::equal), //or
	u32(token_type::equal), //xor
	u32(token_type::equal), //gt
	u32(token_type::equal), //lt
	u32(token_type::equal), //colon
	u32(token_type::logical_eq), //equal
	u32(token_type::equal), //not
	u32(token_type::equal), //dot
	u32(token_type::equal), //mod
	u32(token_type::equal), //eof
	LEX_STATE_INVALID_CHAR,
},

// or
{
	u32(token_type::bitwise_or), //whitespace
	u32(token_type::bitwise_or), //digit
	u32(token_type::bitwise_or), //alpha
	u32(token_type::bitwise_or), //token_misc
	u32(token_type::bitwise_or), //single_quote
	u32(token_type::bitwise_or), //double_quote
	u32(token_type::bitwise_or), //forward_slash
	u32(token_type::bitwise_or), //plus
	u32(token_type::bitwise_or), //minus
	u32(token_type::bitwise_or), //times
	u32(token_type::bitwise_or), //and
	u32(token_type::logical_or), //or
	u32(token_type::bitwise_or), //xor
	u32(token_type::bitwise_or), //gt
	u32(token_type::bitwise_or), //lt
	u32(token_type::bitwise_or), //colon
	u32(token_type::bitwise_or_eq), //equal
	u32(token_type::bitwise_or), //not
	u32(token_type::bitwise_or), //dot
	u32(token_type::bitwise_or), //mod
	u32(token_type::bitwise_or), //eof
	LEX_STATE_INVALID_CHAR,
},


// and
{
	u32(token_type::operator_and), //whitespace
	u32(token_type::operator_and), //digit
	u32(token_type::operator_and), //alpha
	u32(token_type::operator_and), //token_misc
	u32(token_type::operator_and), //single_quote
	u32(token_type::operator_and), //double_quote
	u32(token_type::operator_and), //forward_slash
	u32(token_type::operator_and), //plus
	u32(token_type::operator_and), //minus
	u32(token_type::operator_and), //times
	u32(token_type::logical_and), //and
	u32(token_type::operator_and), //or
	u32(token_type::operator_and), //xor
	u32(token_type::operator_and), //gt
	u32(token_type::operator_and), //lt
	u32(token_type::operator_and), //colon
	u32(token_type::operator_and), //equal
	u32(token_type::operator_and), //not
	u32(token_type::operator_and), //dot
	u32(token_type::operator_and), //mod
	u32(token_type::operator_and), //eof
	LEX_STATE_INVALID_CHAR,
},

// not
{
	u32(token_type::logical_not), //whitespace
	u32(token_type::logical_not), //digit
	u32(token_type::logical_not), //alpha
	u32(token_type::logical_not), //token_misc
	u32(token_type::logical_not), //single_quote
	u32(token_type::logical_not), //double_quote
	u32(token_type::logical_not), //forward_slash
	u32(token_type::logical_not), //plus
	u32(token_type::logical_not), //minus
	u32(token_type::logical_not), //times
	u32(token_type::logical_not), //and
	u32(token_type::logical_not), //or
	u32(token_type::logical_not), //xor
	u32(token_type::logical_not), //gt
	u32(token_type::logical_not), //lt
	u32(token_type::logical_not), //colon
	u32(token_type::logical_ne), //equal
	u32(token_type::logical_not), //not
	u32(token_type::logical_not), //dot
	u32(token_type::logical_not), //mod
	u32(token_type::logical_not), //eof
	LEX_STATE_INVALID_CHAR,
},

// xor
{
	u32(token_type::bitwise_xor), //whitespace
	u32(token_type::bitwise_xor), //digit
	u32(token_type::bitwise_xor), //alpha
	u32(token_type::bitwise_xor), //token_misc
	u32(token_type::bitwise_xor), //single_quote
	u32(token_type::bitwise_xor), //double_quote
	u32(token_type::bitwise_xor), //forward_slash
	u32(token_type::bitwise_xor), //plus
	u32(token_type::bitwise_xor), //minus
	u32(token_type::bitwise_xor), //times
	u32(token_type::bitwise_xor), //and
	u32(token_type::bitwise_xor), //or
	u32(token_type::bitwise_xor), //xor
	u32(token_type::bitwise_xor), //gt
	u32(token_type::bitwise_xor), //lt
	u32(token_type::bitwise_xor), //colon
	u32(token_type::bitwise_xor), //equal
	u32(token_type::bitwise_xor), //not
	u32(token_type::bitwise_xor), //dot
	u32(token_type::bitwise_xor), //mod
	u32(token_type::bitwise_xor), //eof
	LEX_STATE_INVALID_CHAR,
},

// mod
{
	u32(token_type::mod), //whitespace
	u32(token_type::mod), //digit
	u32(token_type::mod), //alpha
	u32(token_type::mod), //token_misc
	u32(token_type::mod), //single_quote
	u32(token_type::mod), //double_quote
	u32(token_type::mod), //forward_slash
	u32(token_type::mod), //plus
	u32(token_type::mod), //minus
	u32(token_type::mod), //times
	u32(token_type::mod), //and
	u32(token_type::mod), //or
	u32(token_type::mod), //xor
	u32(token_type::mod), //gt
	u32(token_type::mod), //lt
	u32(token_type::mod), //colon
	u32(token_type::mod), //equal
	u32(token_type::mod), //not
	u32(token_type::mod), //dot
	u32(token_type::mod), //mod
	u32(token_type::mod), //eof
	LEX_STATE_INVALID_CHAR,
},

// sub
{
	u32(token_type::minus), //whitespace
	LEX_STATE_INT_FIN, //digit
	u32(token_type::minus), //alpha
	u32(token_type::minus), //token_misc
	u32(token_type::minus), //single_quote
	u32(token_type::minus), //double_quote
	u32(token_type::minus), //forward_slash
	u32(token_type::minus), //plus
	u32(token_type::decrement), //minus
	u32(token_type::minus), //times
	u32(token_type::minus), //and
	u32(token_type::minus), //or
	u32(token_type::minus), //xor
	u32(token_type::minus), //gt
	u32(token_type::minus), //lt
	u32(token_type::minus), //colon
	u32(token_type::minus_eq), //equal
	u32(token_type::minus), //not
	u32(token_type::minus), //dot
	u32(token_type::minus), //mod
	u32(token_type::minus), //eof
	LEX_STATE_INVALID_CHAR,
},


// add
{
	u32(token_type::plus), //whitespace
	u32(token_type::plus), //digit
	u32(token_type::plus), //alpha
    u32(token_type::plus), //misc
	u32(token_type::plus), //single_quote
	u32(token_type::plus), //double_quote
	u32(token_type::plus), //forward_slash
	u32(token_type::increment), //plus
	u32(token_type::plus), //minus
	u32(token_type::plus), //times
	u32(token_type::plus), //and
	u32(token_type::plus), //or
	u32(token_type::plus), //xor
	u32(token_type::plus), //gt
	u32(token_type::plus), //lt
	u32(token_type::plus), //colon
	u32(token_type::plus_eq), //equal
	u32(token_type::plus), //not
	u32(token_type::plus), //dot
	u32(token_type::plus), // mod
	u32(token_type::plus), //eof
	LEX_STATE_INVALID_CHAR, //error
},


};
%option noyywrap nounput noinput

%{
    #include "bison.tab.h"
    #include "parser/error.h"

    extern FILE *yyin;
    extern YYLTYPE yylloc;

    int line_num = 1;
    int column_num = 1;

    #define YY_USER_ACTION                              \
        yylloc.first_line = line_num;                   \
        yylloc.first_column = column_num;               \
        yylloc.last_line = line_num;                    \
        yylloc.last_column = column_num + yyleng - 1;   \
        column_num += yyleng;
        
    void yyerror(const char *msg);
%}

DIGIT       [0-9]
FRACTION    {DIGIT}+"."{DIGIT}*
EXPONENT    [eE][-+]?{DIGIT}+
FLT         ({DIGIT}+{FRACTION}? | {FRACTION})({EXPONENT})?
NUM         {DIGIT}+

HEXDIGIT    [0-9a-fA-F]
HEX         0[xX]{HEXDIGIT}+

%%

\n                      { line_num++; column_num = 1; }

\+ |
\- |
\* |
\/ |
\( |
\) |
\{ |
\} |
\; |
\: |
\, |
\| |
\^ |
\& |
\> |
\< |
\? |
\! |
\. |
\[ |
\] |
\~ | 
\= { return yytext[0]; }

"||"                    { return OR; }
"&&"                    { return AND; }
"<<"                    { return LSHIFT; }
">>"                    { return RSHIFT; }
"<="                    { return LESS_EQ; }
"=="                    { return IS_EQUAL; }
"++"                    { return INCREMENT; }
"--"                    { return DECREMENT; }
"!="                    { return NOT_EQUAL; }
">="                    { return GREATER_EQ; }
"|="                    { return OR_ASSIGN; }
"*="                    { return MUL_ASSIGN; }
"/="                    { return DIV_ASSIGN; }
"%="                    { return MOD_ASSIGN; }
"+="                    { return ADD_ASSIGN; }
"-="                    { return SUB_ASSIGN; }
"<<="                   { return SHL_ASSIGN; } 
">>="                   { return SHR_ASSIGN; }  
"&="                    { return AND_ASSIGN; }
"^="                    { return XOR_ASSIGN; }
"=>"                    { return FUN_ARROW; }
"->"                    { return FORWARD_ARROW; }
"<-"                    { return BACKWARD_ARROW; }

"int"                   { return S32; }
"s8"                    { return S8; }
"s16"                   { return S16; }
"s32"                   { return S32; }
"s64"                   { return S64; }
"u8"                    { return U8; }
"u16"                   { return U16; }
"u32"                   { return U32; }
"u64"                   { return U64; }
"f32"                   { return F32; }
"f64"                   { return F64; }
"char"                  { return T_CHAR; }
"void"                  { return T_VOID; }
"bool"                  { return T_BOOLEAN; }
"string"                { return T_STRING; }

"if"                    { return IF; }
"do"                    { return DO; }
"let"                   { return LET; }
"fun"                   { return FUN; }
"for"                   { return FOR; }
"type"                  { return TYPE; }
"mut"                   { return MUT; }
"const"                 { return CONST; }
"else"                  { return ELSE; }
"case"                  { return CASE; }
"enum"                  { return ENUM; }
"true"                  { return TRUE_VAL; }
"false"                 { return FALSE_VAL; }
"break"                 { return BREAK; }
"while"                 { return WHILE; }
"union"                 { return UNION; }
"match"                 { return MATCH; }
"struct"                { return STRUCT; }
"return"                { return RETURN; }
"switch"                { return SWITCH; }
"methods"               { return METHODS; }
"default"               { return DEFAULT; }
"continue"              { return CONTINUE; }

{NUM}                   { yylval.num = atoi(yytext); return NUMBER; }
{HEX}                   { yylval.ln = strtol(yytext, NULL, 16); return HEX; }

\'[ -~]\'               { yylval.ch = yytext[1]; return CHAR; }
[a-zA-Z_][a-zA-Z0-9_]*  { yylval.str = strdup(yytext); return WORD; }
\"([^\\\"\n]|\\.)*\"    { yylval.str = strdup(yytext); return STRING; }

"//".*
"/*"([^*]|\*+[^*/])*"*/"
[ \t]+
.                       { yyerror(SYNTAX_ERROR "Invalid token."); }

%%
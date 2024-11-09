%{
#include <stdio.h>
#include <stdarg.h>

#include "file/path.h"
#include "parser/ptree.h"
#include "parser/error.h"
#include "parser/module.h"

int yylex();
void yyerror(const char *msg);

ptree_t *ptree_g = NULL;
%}
          
%define parse.error verbose      
%locations   

%union {
    int num;
    char ch;
    long ln;
    char *str;
    double db;
    struct ptree *ptree;
}

%token <num> NUMBER
%token <db>  FLOAT
%token <str> STRING
%token <str> WORD
%token <ch>  CHAR
%token <ln>  HEX

%token<num>     OR AND LSHIFT RSHIFT IS_EQUAL LESS_EQ GREATER_EQ NOT_EQUAL INCREMENT DECREMENT FORWARD_ARROW
                OR_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN SHL_ASSIGN SHR_ASSIGN
                AND_ASSIGN XOR_ASSIGN BACKWARD_ARROW FUN_ARROW SCOPE

%token<num>     LET MUT FUN RETURN IF ELSE BREAK CONTINUE DO FOR WHILE SWITCH CASE DEFAULT TYPE CONST STRUCT UNION TRUE_VAL
                ENUM METHODS MATCH FALSE_VAL CLASS EXPORT MODULE IMPORT

%token<num>     S8 S16 S32 S64 U8 U16 U32 U64 F32 F64 T_STRING T_BOOLEAN T_VOID T_CHAR   

%type<ptree>    expression expression_statement assignment_expression statement source_element source_elements
                block_statement return_statement selection_statement jump_statement break_statement continue_statement
                additive_operator multiplicative_operator primary_expression literal assignment_operator iteration_statement
                equality_operator relational_operator shift_operator conditional_expression logical_or_expression
                logical_and_expression bitwise_or_expression bitwise_xor_expression bitwise_and_expression 
                equality_expression relational_expression shift_expression additive_expression multiplicative_expression
                unary_expression postfix_expression unary_operator postfix_operator labeled_statement constant_expression
                for_initialization optional_expression variable_statement variable_list variable arguments property_name
                identifier type primitive_type array_type tuple_type function_type types_list type_statement type_reference
                typed_arguments typed_parameters type_params function_declaration parameters_list type_annotation struct_statement
                field_list methods_statement method_list union_statement enum_statement enum_members enum_member array_literal
                element_list object_literal keyvalue_list keyvalue function_expression function_expression_body parameter
                parameter_is_mut class_statement export_statement module_statement import_statement qualified_identifier

%nonassoc '(' ')'

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

program:
    source_elements {
        ptree_t *f = ptree_create(PTREE_EOF, 0);
        ptree_g = ptree_add($1, 1, f);
    }
    | /* empty */ {
        ptree_g = ptree_create(PTREE_EOF, 0);
    }
    ;

source_elements:
    source_elements source_element {
        $$ = ptree_add($1, 1, $2);
    }
    | source_element {
        $$ = ptree_create(PTREE_SOURCE_ELEMENTS, 1, $1);
    }
    ;

source_element:
    function_declaration {
        $$ = $1;
    }
    | statement {
        $$ = $1;
    }
    ;

function_declaration:
    FUN WORD typed_parameters '(' parameters_list ')' type_annotation  block_statement {
        ptree_t *w = ptree_create_symbol($2);
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        $$ = ptree_create(PTREE_FUNCTION_DECLARATION, 7, w, $3, lp, $5, rp, $7, $8);
    }
    ;

parameters_list:
    parameter_is_mut {
        $$ = ptree_create(PTREE_PARAMETER_LIST, 1, $1);
    }
    | parameters_list ',' parameter_is_mut {
        $$ = ptree_add($1, 1, $3);
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);     
    } 
    ;

parameter_is_mut:
    parameter {
        $$ = ptree_create(PTREE_PARAMETER_CONST, 1, $1);
    }
    | MUT parameter {
        $$ = ptree_create(PTREE_PARAMETER_MUT, 1, $2);
    }
    ;

parameter:
    identifier {
        $$ = ptree_create(PTREE_PARAMETER_UNASSIGNED, 1, $1);
    }
    | identifier '=' assignment_expression {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        $$ = ptree_create(PTREE_PARAMETER_ASSIGNED, 3, $1, eq, $3);
    }
    ;

statement:
    expression_statement {
        $$ = $1;
    }
    | block_statement {
        $$ = $1;
    }
    | jump_statement {
        $$ = $1;
    }
    | labeled_statement {
        $$ = $1;
    }
    | selection_statement {
        $$ = $1;
    }
    | iteration_statement {
        $$ = $1;
    }
    | variable_statement ';' {
        $$ = $1;
    }
    | type_statement {
        $$ = $1;
    }
    | struct_statement {
        $$ = $1;
    }
    | union_statement {
        $$ = $1;
    }
    | enum_statement {
        $$ = $1;
    }
    | methods_statement {
        $$ = $1;
    }
    | class_statement {
        $$ = $1;
    }
    | export_statement {
        $$ = $1;
    }
    | import_statement {
        $$ = $1;
    }
    | module_statement {
        $$ = $1;
    }
    ;


expression_statement:
    optional_expression ';' {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        $$ = ptree_create(PTREE_EXPRESSION_STATEMENT, 2, $1, sc);
    }
    ;

expression:
    assignment_expression {
        $$ = $1;
    }
    | expression ',' assignment_expression {
        $$ = ptree_add($1, 2, $3);
    }
    ;

optional_expression:
    expression {
        $$ = $1;
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);
    }
    ;

assignment_expression:
    unary_expression assignment_operator assignment_expression {
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, $2, $3);
    }
    | function_expression {
        $$ = $1;
    }
    | conditional_expression {
        $$ = $1;
    }
    ;

assignment_operator:
    '=' {
        $$ = ptree_create(PTREE_ASSIGN, 0);
    }
    | OR_ASSIGN {
        $$ = ptree_create(PTREE_OR_ASSIGN, 0);
    }
    | MUL_ASSIGN {
        $$ = ptree_create(PTREE_MUL_ASSIGN, 0);
    }
    | DIV_ASSIGN {
        $$ = ptree_create(PTREE_DIV_ASSIGN, 0);
    }
    | MOD_ASSIGN {
        $$ = ptree_create(PTREE_MOD_ASSIGN, 0);
    }
    | ADD_ASSIGN {
        $$ = ptree_create(PTREE_ADD_ASSIGN, 0);
    }
    | SUB_ASSIGN {
        $$ = ptree_create(PTREE_SUB_ASSIGN, 0);
    }
    | SHL_ASSIGN {
        $$ = ptree_create(PTREE_SHL_ASSIGN, 0);
    }
    | SHR_ASSIGN {
        $$ = ptree_create(PTREE_SHR_ASSIGN, 0);
    }
    | AND_ASSIGN {
        $$ = ptree_create(PTREE_AND_ASSIGN, 0);
    }
    | XOR_ASSIGN {
        $$ = ptree_create(PTREE_XOR_ASSIGN, 0);
    }
    ;

function_expression:
    FUN typed_parameters '(' parameters_list ')' type_annotation FORWARD_ARROW function_expression_body {
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        ptree_t *fa = ptree_create(PTREE_FORWARD_ARROW, 0);
        $$ = ptree_create(PTREE_FUNCTION_EXPRESSION, 7, $2, lp, $4, rp, $6, fa, $8);
    }
    ;

function_expression_body:
    assignment_expression {
        $$ = $1;
    }
    | block_statement {
        $$ = $1;
    }
    ;

constant_expression:
    conditional_expression {
        $$ = $1;
    }
    ;

conditional_expression:
    logical_or_expression {
        $$ = $1;
    }
    | logical_or_expression '?' assignment_expression ':' assignment_expression {
        ptree_t *q = ptree_create(PTREE_QUESTION, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        $$ = ptree_create(PTREE_TERNARY_EXPRESSION, 5, $1, q, $3, sc, $5);
    }
    ;

logical_or_expression:
    logical_and_expression {
        $$ = $1;
    }
    | logical_or_expression OR logical_and_expression {
        ptree_t *op = ptree_create(PTREE_OR, 0);
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, op, $3);
    }
    ;

logical_and_expression:
    bitwise_or_expression {
        $$ = $1;
    }
    | logical_and_expression AND bitwise_or_expression {
        ptree_t *op = ptree_create(PTREE_AND, 0);
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, op, $3);
    }
    ;

bitwise_or_expression:
    bitwise_xor_expression {
        $$ = $1;
    }
    | bitwise_or_expression '|' bitwise_xor_expression {
        ptree_t *op = ptree_create(PTREE_BIT_OR, 0);
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, op, $3);
    }
    ;

bitwise_xor_expression:
    bitwise_and_expression {
        $$ = $1;
    }
    | bitwise_xor_expression '^' bitwise_and_expression {
        ptree_t *op = ptree_create(PTREE_BIT_XOR, 0);
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, op, $3);
    } 
    ;

bitwise_and_expression:
    equality_expression {
        $$ = $1;
    }
    | bitwise_and_expression '&' equality_expression {
        ptree_t *op = ptree_create(PTREE_BIT_AND, 0);
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, op, $3);
    }
    ;

equality_expression:
    relational_expression {
        $$ = $1;
    }
    | equality_expression equality_operator relational_expression {
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, $2, $3);
    }
    ;

equality_operator:
    IS_EQUAL {
        $$ = ptree_create(PTREE_IS_EQUAL, 0);
    }
    | NOT_EQUAL {
        $$ = ptree_create(PTREE_NOT_EQUAL, 0);
    }
    ;

relational_expression:
    shift_expression {
        $$ = $1;
    }
    | relational_expression relational_operator shift_expression {
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, $2, $3);
    }
    ;

relational_operator:
    '<' {
        $$ = ptree_create(PTREE_LESS, 0);
    }
    | '>' {
        $$ = ptree_create(PTREE_GREATER, 0);
    }
    | LESS_EQ {
        $$ = ptree_create(PTREE_LESS_EQ, 0);
    }
    | GREATER_EQ {
        $$ = ptree_create(PTREE_GREATER_EQ, 0);
    }
    ;

shift_expression:
    additive_expression {
        $$ = $1;
    }
    | shift_expression shift_operator additive_expression {
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, $2, $3);
    }
    ;

shift_operator:
    LSHIFT {
        $$ = ptree_create(PTREE_LSHIFT, 0);
    }
    | RSHIFT {
        $$ = ptree_create(PTREE_RSHIFT, 0);
    }
    ;

additive_expression:
    multiplicative_expression {
        $$ = $1;
    }
    | additive_expression additive_operator multiplicative_expression {
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, $2, $3);
    }
    ;

additive_operator:
    '+' {
        $$ = ptree_create(PTREE_PLUS, 0);
    }
    | '-' {
        $$ = ptree_create(PTREE_MINUS, 0);
    }
    ;

multiplicative_expression:
    unary_expression {
        $$ = $1;
    }
    | multiplicative_expression multiplicative_operator unary_expression {
        $$ = ptree_create(PTREE_BINARY_EXPRESSION, 3, $1, $2, $3);
    }
    ;

multiplicative_operator:
    '*' {
        $$ = ptree_create(PTREE_MULTIPLY, 0);
    }
    | '/' {
        $$ = ptree_create(PTREE_DIVIDE, 0);
    }
    | '%' {
        $$ = ptree_create(PTREE_MODULO, 0);
    }
    ;

unary_expression:
    postfix_expression {
        $$ = $1;
    }
    | unary_operator unary_expression {
        $$ = ptree_create(PTREE_UNARY, 2, $1, $2);
    }
    ;

unary_operator:
    INCREMENT {
        $$ = ptree_create(PTREE_INCREMENT, 0);
    }
    | DECREMENT {
        $$ = ptree_create(PTREE_DECREMENT, 0);
    }
    | '!' {
        $$ = ptree_create(PTREE_NOT, 0);
    }
    | '~' {
        $$ = ptree_create(PTREE_TILDE, 0);
    }
    | '+' {
        $$ = ptree_create(PTREE_PLUS, 0);
    }
    | '-' {
        $$ = ptree_create(PTREE_MINUS, 0);
    }
    | '&' {
        $$ = ptree_create(PTREE_BIT_AND, 0);
    }
    ;

postfix_expression:
    primary_expression {
        $$ = $1;
    }
    | postfix_expression postfix_operator {
        $$ = ptree_create(PTREE_POSTFIX, 2, $2, $1);
    }
    | postfix_expression '.' WORD {
        ptree_t *w = ptree_create_word($3);
        ptree_t *dot = ptree_create(PTREE_DOT, 0);
        $$ = ptree_create(PTREE_OBJECT_ACCESS, 3, $1, dot, w);
    }
    | postfix_expression '[' expression ']' {
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        $$ = ptree_create(PTREE_ARRAY_ACCESS, 3, $1, lb, $3, rb);
    }
    | postfix_expression '(' arguments ')' {
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        $$ = ptree_create(PTREE_FUNCTION_CALL, 4, $1, lb, $3, rb);
    }
    ;

arguments:
    assignment_expression {
        $$ = ptree_create(PTREE_ARGUMENT_LIST, 1, $1);
    }
    | arguments ',' assignment_expression {
        $$ = ptree_add($1, 1, $3);
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);
    }
    ;

postfix_operator:
    INCREMENT {
        $$ = ptree_create(PTREE_INCREMENT, 0);
    }
    | DECREMENT {
        $$ = ptree_create(PTREE_DECREMENT, 0);
    }
    ;

primary_expression:
    qualified_identifier {
        $$ = $1;
    }
    | literal {
        $$ = $1;
    }
    | array_literal {
       $$ = $1; 
    }
    | object_literal {
        $$ = $1;
    }
    | '(' expression ')' {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        $$ = ptree_create(PTREE_EXPRESSION, 3, left, $2, right);
    }
    ;

qualified_identifier:
    WORD {
        ptree_t *symbol =  ptree_create_word($1);
        $$ = ptree_create(PTREE_SYMBOL, 1, symbol);
    }
    | qualified_identifier SCOPE WORD {
        ptree_t *symbol =  ptree_create_word($3);
        $$ = ptree_add($1, 1, symbol);
    }
    | SCOPE WORD {
        ptree_t *root =  ptree_create_word("root");
        ptree_t *symbol =  ptree_create_word($2);
        $$ = ptree_create(PTREE_SYMBOL, 2, root, symbol);
    }
    ;

literal:
    NUMBER {
        $$ = ptree_create_num($1);
    }
    | TRUE_VAL {
        $$ = ptree_create_bool(1);
    }
    | FALSE_VAL {
        $$ = ptree_create_bool(0);
    }
    | STRING {
        $$ = ptree_create_str($1);
    }
    ;

array_literal:
    '[' element_list ']' {
        ptree_t *left = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *right = ptree_create(PTREE_RBRACKET, 0);
        $$ = ptree_create(PTREE_ARRAY_LITERAL, 3, left, $2, right);
    }
    ;

element_list:
    assignment_expression {
        $$ = ptree_create(PTREE_ELEMENT_LIST, 1, $1);
    }
    | element_list ',' assignment_expression {
        $$ = ptree_add($1, 1, $3);
    }
    | element_list ',' {
        $$ = $1;
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);
    } 
    ;

object_literal:
    qualified_identifier '{' keyvalue_list '}' {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        $$ = ptree_create(PTREE_OBJECT_LITERAL, 4, $1, left, $3, right);
    }
    ;

keyvalue_list:
    keyvalue {
        $$ = ptree_create(PTREE_KEYVALUE_LIST, 1, $1);
    }
    | keyvalue_list ',' keyvalue {
        $$ = ptree_add($1, 1, $3);
    }
    | keyvalue_list ',' {
        $$ = $1;
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);
    }
    ;

keyvalue:
    property_name ':' assignment_expression {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        $$ = ptree_create(PTREE_KEYVALUE, 3, $1, c, $3);
    }
    ;

property_name:
    WORD {
        $$ = ptree_create_word($1);
    }
    | STRING {
        $$ = ptree_create_str($1);
    }
    | NUMBER {
        $$ = ptree_create_num($1);
    } 
    ;

block_statement:
    '{' source_elements '}' {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        $$ = ptree_create(PTREE_BLOCK, 3, left, $2, right);
    }
    | '{' '}' {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        $$ = ptree_create(PTREE_BLOCK, 3, left, empty, right);
    }
    ;

jump_statement:
    return_statement {
        $$ = $1;
    }
    | break_statement {
        $$ = $1;
    }
    | continue_statement {
        $$ = $1;
    }

return_statement:
    RETURN ';' {
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        $$ = ptree_create(PTREE_RETURN, 1, empty);
    }
    | RETURN expression ';' {
        $$ = ptree_create(PTREE_RETURN, 1, $2);
    }
    ;

break_statement:
    BREAK ';' {
        $$ = ptree_create(PTREE_BREAK, 0);
    }
    ;

continue_statement:
    CONTINUE ';' {
        $$ = ptree_create(PTREE_CONTINUE, 0);
    }
    ;

labeled_statement:
    WORD ':' statement {
        ptree_t *w = ptree_create_word($1);
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        $$ = ptree_create(PTREE_LABEL, 3, w, c, $3);
    }
    | CASE constant_expression ':' statement {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        $$ = ptree_create(PTREE_CASE_LABEL, 3, $2, c, $4);
    }
    | DEFAULT ':' statement {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        $$ = ptree_create(PTREE_DEFAULT_LABEL, 2, c, $3);
    }
    ;

selection_statement:
    IF '(' expression ')' statement %prec LOWER_THAN_ELSE
    {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        $$ = ptree_create(PTREE_IF, 4, left, $3, right, $5);
    }
    | IF '(' expression ')' statement ELSE statement {
        ptree_t *e = ptree_create(PTREE_ELSE, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        $$ = ptree_create(PTREE_IF_ELSE, 6, left, $3, right, $5, e, $7);
    }
    | SWITCH '(' expression ')' statement {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        $$ = ptree_create(PTREE_SWITCH, 4, left, $3, right, $5);
    }
    ;

iteration_statement:
    WHILE '(' expression ')' statement {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        $$ = ptree_create(PTREE_WHILE, 4, left, $3, right, $5);
    }
    | DO statement WHILE '(' expression ')' ';' {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *wh = ptree_create(PTREE_WHILE, 0);

        $$ = ptree_create(PTREE_DO, 6, $2, wh, left, $5, right, sc);
    }
    | FOR '(' for_initialization ';' optional_expression ';' optional_expression ')' statement {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *sc2 = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        $$ = ptree_create(PTREE_FOR, 8, left, $3, sc, $5, sc2, $7, right, $9);
    }
    ;

for_initialization:
    optional_expression {
        $$ = $1;
    }
    | variable_statement {
        $$ = $1;
    }
    ;

variable_statement:
    LET variable_list {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        $$ = ptree_create(PTREE_LET_STATEMENT, 2, $2, sc);
    }
    | CONST variable_list {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        $$ = ptree_create(PTREE_CONST_STATEMENT, 2, $2, sc);
    }
    ;

variable_list:
    variable {
        $$ = ptree_create(PTREE_VARIABLE_LIST, 1, $1);
    }
    | variable_list ',' variable {
        $$ = ptree_add($1, 1, $3);
    }
    ;

variable:
    identifier {
        $$ = ptree_create(PTREE_VARIABLE_UNASSIGNED, 1, $1);
    }
    | identifier '=' assignment_expression {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        $$ = ptree_create(PTREE_VARIABLE_ASSIGNED, 3, $1, eq, $3);
    }
    ;

identifier:
    WORD type_annotation {
        ptree_t *w = ptree_create_symbol($1);
        $$ = ptree_create(PTREE_IDENTIFIER, 2, w, $2);
    }
    ;

type_annotation:
    ':' type {
        ptree_t *sc = ptree_create(PTREE_COLON, 0);
        $$ = ptree_create(PTREE_TYPE_ANNOTATION, 2, sc, $2); 
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);     
    }
    ;

type:
    primitive_type {
        $$ = $1;
    }
    | array_type {
        $$ = $1;
    }
    | tuple_type {
        $$ = $1;
    }
    | function_type {
        $$ = $1;        
    }
    | type_reference {
        $$ = $1;
    }
    ;

primitive_type:
    T_VOID { 
        $$ = ptree_create(PTREE_VOID_TYPE, 0);
    }
    | S32 {
        $$ = ptree_create(PTREE_INTEGER_TYPE, 0);
    }
    | T_STRING {
        $$ = ptree_create(PTREE_STRING_TYPE, 0);
    }
    | T_BOOLEAN {
        $$ = ptree_create(PTREE_BOOLEAN_TYPE, 0);
    }
    ;

array_type:
    type '[' ']' {
        ptree_t *lb = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb = ptree_create(PTREE_RBRACKET, 0);
        $$ = ptree_create(PTREE_ARRAY_TYPE, 3, $1, lb, rb);
    }
    ;

tuple_type:
    type '[' NUMBER ']' {
        ptree_t *num = ptree_create_num($3);
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        $$ = ptree_create(PTREE_TUPLE_TYPE, 4, $1, lb, num, rb);
    }
    ;

function_type:
    type BACKWARD_ARROW '(' types_list ')' {
        ptree_t *a = ptree_create(PTREE_BACKWARD_ARROW, 0);
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        $$ = ptree_create(PTREE_FUNCTION_TYPE, 5, $1, a, lb, $4, rb);
    }
    ;

types_list:
    type {
        $$ = ptree_create(PTREE_TYPE_LIST, 1, $1);
    }
    | types_list ',' type {
        $$ = ptree_add($1, 1, $3);
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0); 
    }
    ;

type_reference:
    WORD {
        ptree_t *w = ptree_create_symbol($1);
        $$ = ptree_create(PTREE_TYPE_REFERENCE, 1, w);
    }
    | WORD typed_arguments {
        ptree_t *w = ptree_create_symbol($1);
        $$ = ptree_create(PTREE_TYPE_REFERENCE, 2, w, $2);
    }
    ;

typed_arguments:
    '<' types_list '>' {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        $$ = ptree_create(PTREE_TYPED_ARGUMENTS, 3, l, $2, g);
    }
    ;

typed_parameters:
    '<' type_params '>' {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        $$ = ptree_create(PTREE_TYPED_PARAMETERS, 3, l, $2, g); 
    }
    | /* empty */ {
         $$ = ptree_create(PTREE_EMPTY, 0);     
    }
    ;

type_params:
    WORD {
        ptree_t *w = ptree_create_symbol($1);
        $$ = ptree_create(PTREE_TYPE_PARAMS, 1, w); 
    }
    | type_params ',' WORD {
        ptree_t *w = ptree_create_symbol($3);
        $$ = ptree_add($1, 1, w);
    }   
    ;

type_statement:
    TYPE WORD typed_parameters '=' type ';' {
        ptree_t *w = ptree_create_symbol($2);
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        $$ = ptree_create(PTREE_TYPE_ALIAS, 5, w, $3, eq, $5, sc); 
    }
    ;

struct_statement:
    STRUCT WORD ';' {
        ptree_t *w = ptree_create_symbol($2);
        $$ = ptree_create(PTREE_UNIT_STRUCT, 1, w);
    }
    | STRUCT WORD typed_parameters '{' field_list '}' {
        ptree_t *w = ptree_create_symbol($2);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        $$ = ptree_create(PTREE_STRUCT, 5, w, $3, left, $5, right);
    }
    ;

union_statement:
    UNION WORD typed_parameters '{' field_list '}' {
        ptree_t *w = ptree_create_symbol($2);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        $$ = ptree_create(PTREE_UNION, 5, w, $3, left, $5, right);
    }
    ;

field_list:
    identifier {
        $$ = ptree_create(PTREE_FIELDS, 1, $1);
    }
    | field_list ',' identifier {
        $$ = ptree_add($1, 1, $3);
    }
    | field_list ',' {
        $$ = $1;
    }
    | /* empty */ {
        $$ = ptree_create(PTREE_EMPTY, 0);     
    }
    ;

enum_statement:
    ENUM WORD typed_parameters '{' enum_members '}' {
        ptree_t *w = ptree_create_symbol($2);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        $$ = ptree_create(PTREE_ENUM, 5, w, $3, left, $5, right);
    }
    ;

enum_members:
    enum_member {
        $$ = ptree_create(PTREE_ENUM_MEMBERS, 1, $1);
    }
    | enum_members ',' enum_member {
        $$ = ptree_add($1, 1, $3);
    }
    | enum_members ',' {
        $$ = $1;
    }
    ;

enum_member:
    WORD {
        $$ = ptree_create_word($1);
    }
    | WORD '(' types_list ')' {
        ptree_t *w = ptree_create_word($1);
        ptree_t *left = ptree_create(PTREE_LPRN, 0);
        ptree_t *right = ptree_create(PTREE_RPRN, 0);
        $$ = ptree_create(PTREE_ENUM_STRUCT, 4, w, left, $3, right);
    }
    | WORD '{' field_list '}' {
        ptree_t *w = ptree_create_word($1);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        $$ = ptree_create(PTREE_ENUM_NAMED_STRUCT, 4, w, left, $3, right);
    }
    | WORD '=' NUMBER {
        ptree_t *w = ptree_create_word($1);
        ptree_t *num = ptree_create_num($3);
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        $$ = ptree_create(PTREE_ENUM_DEFAULT, 3, w, eq, num);
    }
    ;

methods_statement:
    METHODS qualified_identifier '{' method_list '}' {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        $$ = ptree_create(PTREE_METHODS, 4, $2, left, $4, right);
    }
    ;

method_list:
    function_declaration {
        $$ = ptree_create(PTREE_METHODS_LIST, 1, $1);
    }
    | method_list function_declaration {
        $$ = ptree_add($1, 1, $2);
    }
    ;

class_statement:
    CLASS WORD typed_parameters '{' field_list '}' {
        ptree_t *w = ptree_create_symbol($2);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        $$ = ptree_create(PTREE_CLASS, 5, w, $3, left, $5, right);
    }
    ;

export_statement:
    EXPORT function_declaration {
        $$ = ptree_create(PTREE_EXPORT, 1, $2);
    }
    | EXPORT struct_statement {
        $$ = ptree_create(PTREE_EXPORT, 1, $2);
    }
    | EXPORT module_statement {
        $$ = ptree_create(PTREE_EXPORT, 1, $2);
    }
    ;

import_statement:
    IMPORT WORD ';' {
        ptree_t *w = ptree_create_symbol($2);
        $$ = ptree_create(PTREE_IMPORT, 1, w);
    }
    ;

module_statement:
    MODULE WORD '{' source_elements '}' {
        ptree_t *w = ptree_create_symbol($2);
        $$ = ptree_create(PTREE_MODULE, 2, w, $4);
    }
    ;

%%
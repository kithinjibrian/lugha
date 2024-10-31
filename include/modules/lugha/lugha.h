#ifndef LUGHA_H
#define LUGHA_H

#include "parser/error.h"
#include "modules/default/ast.h"
#include "modules/default/visitor.h"

extern char *filename_g;
extern const char *ast_type_str_g[];

define_visitor(lugha_do, node_do_t);
define_visitor(lugha_eof, node_ast_t);
define_visitor(lugha_break, node_ast_t);
define_visitor(lugha_array, node_array_t);
define_visitor(lugha_block, node_block_t);
define_visitor(lugha_unary, node_unary_t);
define_visitor(lugha_while, node_while_t);
define_visitor(lugha_boolean, node_bool_t);
define_visitor(lugha_continue, node_ast_t);
define_visitor(lugha_number, node_number_t);
define_visitor(lugha_symbol, node_symbol_t);
define_visitor(lugha_string, node_string_t);
define_visitor(lugha_return, node_return_t);
define_visitor(lugha_if_else, node_if_else_t);
define_visitor(lugha_ternary, node_ternary_t);
define_visitor(lugha_postfix, node_postfix_t);
define_visitor(lugha_variable, node_variable_t);
define_visitor(lugha_call, node_function_call_t);
define_visitor(lugha_arguments, node_arguments_t);
define_visitor(lugha_parameter, node_parameter_t);
define_visitor(lugha_identifier, node_identifier_t);
define_visitor(lugha_expression, node_expression_t);
define_visitor(lugha_binary_expression, node_binary_t);
define_visitor(lugha_function_dec, node_function_dec_t);
define_visitor(lugha_array_access, node_array_access_t);
define_visitor(lugha_source_elements, node_statements_t);
define_visitor(lugha_variable_list, node_variable_list_t);
define_visitor(lugha_parameter_list, node_parameter_list_t);
define_visitor(lugha_expression_statement, node_expression_t);
define_visitor(lugha_variable_statement, node_variable_statement_t);
define_visitor(lugha_function_expression, node_function_expression_t);

#endif /* LUGHA_H */
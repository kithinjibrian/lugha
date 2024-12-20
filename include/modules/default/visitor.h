#ifndef VISITOR_H
#define VISITOR_H

#include "ast.h"
#include "macro.h"

#define define_visitor(_name, _type) \
    void *_name(__UNUSED__ module_t *module, node_visitor_t *visitor, _type *ast)

/*
    Default visitors
 */
define_visitor(def_eof, node_ast_t);
define_visitor(def_entry, node_ast_t);
define_visitor(def_leave, node_ast_t);
define_visitor(def_bool, node_bool_t);
define_visitor(def_word, node_word_t);
define_visitor(def_block, node_block_t);
define_visitor(def_array, node_array_t);
define_visitor(def_class, node_class_t);
define_visitor(def_module, node_module_t);
define_visitor(def_object, node_object_t);
define_visitor(def_string, node_string_t);
define_visitor(def_symbol, node_symbol_t);
define_visitor(def_struct, node_struct_t);
define_visitor(def_number, node_number_t);
define_visitor(def_return, node_return_t);
define_visitor(def_import, node_import_t);
define_visitor(def_export, node_export_t);
define_visitor(def_ternary, node_ternary_t);
define_visitor(def_methods, node_methods_t);
define_visitor(def_if_else, node_if_else_t);
define_visitor(def_variable, node_variable_t);
define_visitor(def_arguments, node_arguments_t);
define_visitor(def_parameter, node_parameter_t);
define_visitor(def_expression, node_expression_t);
define_visitor(def_binary_expression, node_binary_t);
define_visitor(def_function_dec, node_function_dec_t);
define_visitor(def_array_access, node_array_access_t);
define_visitor(def_source_elements, node_statements_t);
define_visitor(def_function_call, node_function_call_t);
define_visitor(def_object_access, node_object_access_t);
define_visitor(def_variable_list, node_variable_list_t);
define_visitor(def_parameter_list, node_parameter_list_t);
define_visitor(def_expression_statement, node_expression_t);
define_visitor(def_variable_statement, node_variable_statement_t);
define_visitor(def_function_expression, node_function_expression_t);

#endif /* VISITOR_H */
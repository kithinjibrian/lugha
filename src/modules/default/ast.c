#include "modules/default/ast.h"

node_visitor_t code_gen;

memory_arena_t ast_arena_g = {NULL};
node_visitor_t *phases_g[] = {
    &sema_visitor,
    &type_visitor,
    NULL,
};

const char *ast_type_str_g[] = {
    DATA_TYPE(LIST_STRINGIFY)};

DEFINE_FREE(ast_exit, &ast_arena_g);
DEFINE_ALLOC(ast_alloc, &ast_arena_g);
DEFINE_STRDUP(ast_strdup, &ast_arena_g);

static node_ast_t *gen_ast(ptree_t *ptree);

DEFINE_ACCEPT(accept_do, node_do_t, do_fun);
DEFINE_ACCEPT(accept_eof, node_ast_t, eof_fun);
DEFINE_ACCEPT(accept_type, node_type_t, type_fun);
DEFINE_ACCEPT(accept_bool, node_bool_t, bool_fun);
DEFINE_ACCEPT(accept_word, node_word_t, word_fun);
DEFINE_ACCEPT(accept_break, node_ast_t, break_fun);
DEFINE_ACCEPT(accept_class, node_class_t, class_fun);
DEFINE_ACCEPT(accept_unary, node_unary_t, unary_fun);
DEFINE_ACCEPT(accept_block, node_block_t, block_fun);
DEFINE_ACCEPT(accept_while, node_while_t, while_fun);
DEFINE_ACCEPT(accept_array, node_array_t, array_fun);
DEFINE_ACCEPT(accept_object, node_object_t, object_fun);
DEFINE_ACCEPT(accept_struct, node_struct_t, struct_fun);
DEFINE_ACCEPT(accept_number, node_number_t, number_fun);
DEFINE_ACCEPT(accept_string, node_string_t, string_fun);
DEFINE_ACCEPT(accept_symbol, node_symbol_t, symbol_fun);
DEFINE_ACCEPT(accept_return, node_return_t, return_fun);
DEFINE_ACCEPT(accept_continue, node_ast_t, continue_fun);
DEFINE_ACCEPT(accept_ternary, node_ternary_t, ternary_fun);
DEFINE_ACCEPT(accept_postfix, node_postfix_t, postfix_fun);
DEFINE_ACCEPT(accept_if_else, node_if_else_t, if_else_fun);
DEFINE_ACCEPT(accept_keyvalue, node_keyvalue_t, keyvalue_fun);
DEFINE_ACCEPT(accept_variable, node_variable_t, variable_fun);
DEFINE_ACCEPT(accept_parameter, node_parameter_t, parameter_fun);
DEFINE_ACCEPT(accept_arguments, node_arguments_t, arguments_fun);
DEFINE_ACCEPT(accept_identifier, node_identifier_t, identifier_fun);
DEFINE_ACCEPT(accept_expression, node_expression_t, expression_fun);
DEFINE_ACCEPT(accept_statements, node_statements_t, statements_fun);
DEFINE_ACCEPT(accept_source_elements, node_statements_t, statements_fun);
DEFINE_ACCEPT(accept_array_access, node_array_access_t, array_access_fun);
DEFINE_ACCEPT(accept_function_dec, node_function_dec_t, function_dec_fun);
DEFINE_ACCEPT(accept_function_call, node_function_call_t, function_call_fun);
DEFINE_ACCEPT(accept_variable_list, node_variable_list_t, variable_list_fun);
DEFINE_ACCEPT(accept_binary_expression, node_binary_t, binary_expression_fun);
DEFINE_ACCEPT(accept_parameter_list, node_parameter_list_t, parameter_list_fun);
DEFINE_ACCEPT(accept_expression_statement, node_expression_t, expression_statement_fun);
DEFINE_ACCEPT(accept_variable_statement, node_variable_statement_t, variable_statement_fun);
DEFINE_ACCEPT(accept_function_expression, node_function_expression_t, function_expression_fun);

static inline node_ast_t *create_vars(ptree_t *ptree, bool is_const)
{
    DEFINE_NODE_AST(
        ptree,
        node_variable_statement_t,
        NODE_VARIABLE_STATEMENT,
        accept_variable_statement);

    ast->statement = gen_ast(ptree->children[0]);
    ast->is_const = is_const;

    if (is_const)
    {
        node_variable_list_t *vars = (node_variable_list_t *)(ast->statement);

        for (int i = 0; i < vars->nvars; i++)
        {
            node_variable_t *var = (node_variable_t *)(vars->variables[i]);
            var->is_const = true;
            ((node_identifier_t *)(var->identifer))->symbol->is_const = true;
        }
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_eof(ptree_t *ptree)
{
    DEFINE_NODE_SIMPLE(ptree, NODE_EOF, accept_eof);
    return node_ast_cast(ast);
}

static inline node_ast_t *_break(ptree_t *ptree)
{
    DEFINE_NODE_SIMPLE(ptree, NODE_BREAK, accept_break);
    return node_ast_cast(ast);
}

static inline node_ast_t *_continue(ptree_t *ptree)
{
    DEFINE_NODE_SIMPLE(ptree, NODE_CONTINUE, accept_continue);
    return node_ast_cast(ast);
}

static inline node_ast_t *_num(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_number_t, NODE_NUMBER, accept_number);
    ast->num = ptree->num;
    return node_ast_cast(ast);
}

static inline node_ast_t *_block(ptree_t *ptree)
{
    enter_scope("block");
    DEFINE_NODE_AST(ptree, node_block_t, NODE_BLOCK, accept_block);
    ast->statements = gen_ast(ptree->children[1]);
    exit_scope();
    return node_ast_cast(ast);
}

static inline node_ast_t *_return(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_return_t, NODE_RETURN, accept_return);
    ast->expression = gen_ast(ptree->children[0]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_symbol(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_symbol_t, NODE_SYMBOL, accept_symbol);
    ast->name = ast_strdup(ptree->str);
    ast->symbol = lookup_symbol(ptree->str);
    return node_ast_cast(ast);
}

static inline node_ast_t *_word(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_word_t, NODE_WORD, accept_word);
    ast->name = ast_strdup(ptree->str);
    return node_ast_cast(ast);
}

static inline node_ast_t *_string(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_string_t, NODE_STRING, accept_string);
    ast->str = ast_strdup(ptree->str);
    return node_ast_cast(ast);
}

static inline node_ast_t *_bool(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_bool_t, NODE_NUMBER, accept_bool);
    ast->bol = ptree->num;
    return node_ast_cast(ast);
}

static inline node_ast_t *_while(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_while_t, NODE_WHILE, accept_while);
    ast->expression = gen_ast(ptree->children[1]);
    ast->statement = gen_ast(ptree->children[3]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_do(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_do_t, NODE_DO, accept_do);
    ast->statement = gen_ast(ptree->children[0]);
    ast->expression = gen_ast(ptree->children[3]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_source_elements(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_statements_t, NODE_STATEMENTS, accept_source_elements);
    ast->nch = ptree->nch;
    ast->children = (node_ast_t **)ast_alloc(sizeof(node_ast_t *) * ptree->nch);

    for (int i = 0; i < ptree->nch; i++)
    {
        ast->children[i] = gen_ast(ptree->children[i]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_variable_assigned(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_variable_t, NODE_VARIABLE, accept_variable);
    ast->identifer = gen_ast(ptree->children[0]);
    ast->expression = gen_ast(ptree->children[2]);
    ast->is_const = false;

    return node_ast_cast(ast);
}

static inline node_ast_t *_variable_unassigned(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_variable_t, NODE_VARIABLE, accept_variable);
    ast->identifer = gen_ast(ptree->children[0]);
    ast->expression = NULL;
    ast->is_const = false;

    return node_ast_cast(ast);
}

static inline node_ast_t *_variable_list(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_variable_list_t, NODE_VARIABLE_LIST, accept_variable_list);
    ast->nvars = ptree->nch;
    ast->variables = (node_ast_t **)ast_alloc(sizeof(node_ast_t *) * ptree->nch);
    for (int i = 0; i < ptree->nch; i++)
    {
        ast->variables[i] = gen_ast(ptree->children[i]);
    }
    return node_ast_cast(ast);
}

static inline node_ast_t *_identifier(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_identifier_t, NODE_IDENTIFIER, accept_identifier);
    ast->name = ptree->children[0]->str;
    ast->symbol = insert_symbol(ast->name);
    ast->symbol->type = SYM_VARIABLE;
    ast->type = gen_ast(ptree->children[1]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_type_annotation(ptree_t *ptree)
{
    return gen_ast(ptree->children[1]);
}

static inline node_ast_t *_type_integer(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_type_t, NODE_TYPE, accept_type);
    ast->tag = TYPE_CON;
    ast->con.name = ast_strdup("int");
    return node_ast_cast(ast);
}

static inline node_ast_t *_type_bool(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_type_t, NODE_TYPE, accept_type);
    ast->tag = TYPE_CON;
    ast->con.name = ast_strdup("bool");
    return node_ast_cast(ast);
}

static inline node_ast_t *_type_string(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_type_t, NODE_TYPE, accept_type);
    ast->tag = TYPE_CON;
    ast->con.name = ast_strdup("string");
    return node_ast_cast(ast);
}

static inline node_ast_t *_type_function(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_type_t, NODE_TYPE, accept_type);
    ast->tag = TYPE_CON;
    ast->con.name = ast_strdup("->");
    return node_ast_cast(ast);
}

static inline node_ast_t *_type_array(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_type_t, NODE_TYPE, accept_type);
    ast->tag = TYPE_CON;
    ast->con.name = ast_strdup("[]");
    return node_ast_cast(ast);
}

static inline node_ast_t *_type_reference(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_type_t, NODE_TYPE, accept_type);
    ast->tag = TYPE_CON;
    ast->con.name = ast_strdup(ptree->children[0]->str);

    printf("TODO: type reference: %s\n", ast->con.name);

    return node_ast_cast(ast);
}

static inline node_ast_t *_expression_statement(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_expression_t, NODE_EXPRESSION, accept_expression_statement);
    ast->expression = gen_ast(ptree->children[0]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_expression(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_expression_t, NODE_EXPRESSION, accept_expression);
    ast->expression = gen_ast(ptree->children[1]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_binary_expression(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_binary_t, NODE_BINARY, accept_binary_expression);
    ast->left = gen_ast(ptree->children[0]);
    ast->right = gen_ast(ptree->children[2]);

    ptree_t *op = ptree->children[1];
    switch (op->type)
    {
    case PTREE_PLUS:
        ast->op = OP_PLUS;
        break;
    case PTREE_MINUS:
        ast->op = OP_MINUS;
        break;
    case PTREE_MULTIPLY:
        ast->op = OP_MULT;
        break;
    case PTREE_DIVIDE:
        ast->op = OP_DIV;
        break;
    case PTREE_MODULO:
        ast->op = OP_MOD;
        break;
    case PTREE_AND:
        ast->op = OP_AND;
        break;
    case PTREE_OR:
        ast->op = OP_OR;
        break;
    case PTREE_BIT_XOR:
        ast->op = OP_BITWISE_XOR;
        break;
    case PTREE_BIT_AND:
        ast->op = OP_BITWISE_AND;
        break;
    case PTREE_BIT_OR:
        ast->op = OP_BITWISE_OR;
        break;
    case PTREE_LESS:
        ast->op = OP_LESS;
        break;
    case PTREE_LESS_EQ:
        ast->op = OP_LESS_EQUAL;
        break;
    case PTREE_GREATER:
        ast->op = OP_GREATER;
        break;
    case PTREE_GREATER_EQ:
        ast->op = OP_GREATER_EQUAL;
        break;
    case PTREE_IS_EQUAL:
        ast->op = OP_IS_EQUAL;
        break;
    case PTREE_NOT_EQUAL:
        ast->op = OP_NOT_EQUAL;
        break;
    case PTREE_LSHIFT:
        ast->op = OP_LSHIFT;
        break;
    case PTREE_RSHIFT:
        ast->op = OP_RSHIFT;
        break;
    case PTREE_ASSIGN:
        ast->op = OP_ASSIGN;
        break;
    default:
        break;
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_function_expression(ptree_t *ptree)
{
    DEFINE_NODE_AST(
        ptree,
        node_function_expression_t,
        NODE_FUNCTION_EXPRESSION,
        accept_function_expression);

    //  enter_scope("lambda");

    ast->typed_params = gen_ast(ptree->children[0]);
    ast->parameters = gen_ast(ptree->children[2]);
    ast->return_type = gen_ast(ptree->children[4]);
    ast->block = gen_ast(ptree->children[6]);

    //  exit_scope();

    return node_ast_cast(ast);
}

static inline node_ast_t *_function_declaration(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_function_dec_t, NODE_FUNCTION_DEC, accept_function_dec);

    ast->name = ast_strdup(ptree->children[0]->str);

    ast->symbol = insert_symbol(ast->name);

    enter_scope(ast->name);

    ast->typed_params = gen_ast(ptree->children[1]);
    ast->parameters = gen_ast(ptree->children[3]);
    ast->return_type = gen_ast(ptree->children[5]);
    ast->block = gen_ast(ptree->children[6]);

    ast->symbol->type = SYM_FUNCTION;
    ast->symbol->no_params = ptree->children[3]->nch;

    exit_scope();

    return node_ast_cast(ast);
}

static inline node_ast_t *_parameter_list(ptree_t *ptree)
{
    DEFINE_NODE_AST(
        ptree,
        node_parameter_list_t,
        NODE_PARAMETER_LIST,
        accept_parameter_list);

    ast->params_no = ptree->nch;
    ast->parameters = (node_ast_t **)ast_alloc(sizeof(node_ast_t *) * ptree->nch);
    for (int i = 0; i < ptree->nch; i++)
    {
        ast->parameters[i] = gen_ast(ptree->children[i]);
    }
    return node_ast_cast(ast);
}

static inline node_ast_t *_parameter_mut(ptree_t *ptree)
{
    DEFINE_NODE_AST(
        ptree,
        node_parameter_t,
        NODE_PARAMETER,
        accept_parameter);

    ast->is_mutable = true;

    ptree_t *param = ptree->children[0];

    ast->identifer = gen_ast(param->children[0]);
    ast->expression = NULL;

    symbol_t *symbol = ((node_identifier_t *)(ast->identifer))->symbol;
    symbol->type = SYM_PARAMETER;

    if (param->type == PTREE_PARAMETER_ASSIGNED)
    {
        ast->expression = gen_ast(param->children[2]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_parameter_const(ptree_t *ptree)
{
    DEFINE_NODE_AST(
        ptree,
        node_parameter_t,
        NODE_PARAMETER,
        accept_parameter);

    ast->is_mutable = false;

    ptree_t *param = ptree->children[0];

    ast->identifer = gen_ast(param->children[0]);
    ast->expression = NULL;

    symbol_t *symbol = ((node_identifier_t *)(ast->identifer))->symbol;
    symbol->is_const = true;
    symbol->type = SYM_PARAMETER;

    if (param->type == PTREE_PARAMETER_ASSIGNED)
    {
        ast->expression = gen_ast(param->children[2]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_function_call(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_function_call_t, NODE_FUNCTION_CALL, accept_function_call);
    ast->expression = gen_ast(ptree->children[0]);
    ast->arguments = gen_ast(ptree->children[2]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_argument_list(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_arguments_t, NODE_ARGUMENTS, accept_arguments);
    ast->nargs = ptree->nch;
    ast->args = (node_ast_t **)ast_alloc(ptree->nch * sizeof(node_ast_t *));
    for (int i = 0; i < ptree->nch; i++)
    {
        ast->args[i] = gen_ast(ptree->children[i]);
    }
    return node_ast_cast(ast);
}

static inline node_ast_t *_unary(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_unary_t, NODE_UNARY, accept_unary);
    ast->expression = gen_ast(ptree->children[1]);
    ptree_t *op = ptree->children[0];
    ast->is_postfix = false;
    switch (op->type)
    {
    case PTREE_NOT:
        ast->op = OP_NOT;
        break;
    case PTREE_INCREMENT:
        ast->op = OP_INCREMENT;
        break;
    case PTREE_DECREMENT:
        ast->op = OP_DECREMENT;
        break;
    case PTREE_MINUS:
        ast->op = OP_MINUS;
        break;
    case PTREE_PLUS:
        ast->op = OP_PLUS;
        break;
    case PTREE_BIT_AND:
        ast->op = OP_BITWISE_AND;
        break;
    default:
        break;
    }
    return node_ast_cast(ast);
}

static inline node_ast_t *_postfix(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_postfix_t, NODE_POSTFIX, accept_postfix);
    ast->expression = gen_ast(ptree->children[1]);

    ptree_t *op = ptree->children[0];

    switch (op->type)
    {
    case PTREE_INCREMENT:
        ast->op = OP_INCREMENT;
        break;
    case PTREE_DECREMENT:
        ast->op = OP_DECREMENT;
        break;
    default:
        break;
    }
    return node_ast_cast(ast);
}

static inline node_ast_t *_if(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_if_else_t, NODE_IF_ELSE, accept_if_else);

    ast->newline = true;
    ast->condition = gen_ast(ptree->children[1]);
    ast->then_block = gen_ast(ptree->children[3]);
    ast->else_block = NULL;

    return node_ast_cast(ast);
}

static inline node_ast_t *_if_else(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_if_else_t, NODE_IF_ELSE, accept_if_else);

    ast->newline = true;
    ast->condition = gen_ast(ptree->children[1]);
    ast->then_block = gen_ast(ptree->children[3]);

    if (ptree->nch == 6)
    {
        ast->else_block = gen_ast(ptree->children[5]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_ternary_expression(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_ternary_t, NODE_TERNARY, accept_ternary);
    ast->condition = gen_ast(ptree->children[0]);
    ast->then_block = gen_ast(ptree->children[2]);
    ast->else_block = gen_ast(ptree->children[4]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_array_literal(ptree_t *ptree)
{
    DEFINE_NODE_AST(
        ptree,
        node_array_t,
        NODE_ARRAY,
        accept_array);

    ptree_t *elements = ptree->children[1];
    ast->nch = elements->nch;
    ast->elements = (node_ast_t **)ast_alloc(sizeof(node_ast_t *) * elements->nch);

    for (int i = 0; i < elements->nch; i++)
    {
        ast->elements[i] = (node_ast_t *)gen_ast(elements->children[i]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_array_access(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_array_access_t, NODE_ARRAY_ACCESS, accept_array_access);
    ast->array = gen_ast(ptree->children[0]);
    ast->index = gen_ast(ptree->children[2]);
    return node_ast_cast(ast);
}

static inline node_ast_t *_class(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_class_t, NODE_CLASS, accept_class);
    return node_ast_cast(ast);
}

static inline node_ast_t *_struct(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_struct_t, NODE_STRUCT, accept_struct);

    ast->symbol = insert_symbol(ast_strdup(ptree->children[0]->str));
    ast->symbol->type = SYM_STRUCT;

    ptree_t *fields = ptree->children[3];

    ast->nch = fields->nch;

    ast->fields = (node_ast_t **)ast_alloc(sizeof(node_ast_t *) * fields->nch);

    for (int i = 0; i < fields->nch; i++)
    {
        ast->fields[i] = gen_ast(fields->children[i]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_object_literal(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_object_t, NODE_OBJECT, accept_object);
    ast->symbol = lookup_symbol(ptree->children[0]->str);

    ptree_t *fields = ptree->children[2];

    ast->nch = fields->nch;
    ast->fields = (node_ast_t **)ast_alloc(sizeof(node_ast_t *) * fields->nch);

    for (int i = 0; i < fields->nch; i++)
    {
        ast->fields[i] = gen_ast(fields->children[i]);
    }

    return node_ast_cast(ast);
}

static inline node_ast_t *_keyvalue(ptree_t *ptree)
{
    DEFINE_NODE_AST(ptree, node_keyvalue_t, NODE_KEYVALUE, accept_keyvalue);

    ast->key = gen_ast(ptree->children[0]);
    ast->value = gen_ast(ptree->children[2]);

    return node_ast_cast(ast);
}

static node_ast_t *gen_ast(ptree_t *ptree)
{
    //  printf("AST: %s\n", ptree_type_str_g[ptree->type]);

    switch (ptree->type)
    {
    case PTREE_CONST_STATEMENT:
        return create_vars(ptree, true);
    case PTREE_LET_STATEMENT:
        return create_vars(ptree, false);
    case PTREE_EMPTY:
        return NULL;
    case PTREE_EOF:
        return _eof(ptree);
    case PTREE_NUM:
        return _num(ptree);
    case PTREE_BLOCK:
        return _block(ptree);
    case PTREE_RETURN:
        return _return(ptree);
    case PTREE_BREAK:
        return _break(ptree);
    case PTREE_CONTINUE:
        return _continue(ptree);
    case PTREE_SYMBOL:
        return _symbol(ptree);
    case PTREE_WORD:
        return _word(ptree);
    case PTREE_STRING:
        return _string(ptree);
    case PTREE_BOOL:
        return _bool(ptree);
    case PTREE_WHILE:
        return _while(ptree);
    case PTREE_DO:
        return _do(ptree);
    case PTREE_SOURCE_ELEMENTS:
        return _source_elements(ptree);
    case PTREE_VARIABLE_ASSIGNED:
        return _variable_assigned(ptree);
    case PTREE_VARIABLE_UNASSIGNED:
        return _variable_unassigned(ptree);
    case PTREE_VARIABLE_LIST:
        return _variable_list(ptree);
    case PTREE_IDENTIFIER:
        return _identifier(ptree);
    case PTREE_TYPE_ANNOTATION:
        return _type_annotation(ptree);
    case PTREE_EXPRESSION_STATEMENT:
        return _expression_statement(ptree);
    case PTREE_EXPRESSION:
        return _expression(ptree);
    case PTREE_BINARY_EXPRESSION:
        return _binary_expression(ptree);
    case PTREE_FUNCTION_EXPRESSION:
        return _function_expression(ptree);
    case PTREE_FUNCTION_DECLARATION:
        return _function_declaration(ptree);
    case PTREE_PARAMETER_LIST:
        return _parameter_list(ptree);
    case PTREE_PARAMETER_MUT:
        return _parameter_mut(ptree);
    case PTREE_PARAMETER_CONST:
        return _parameter_const(ptree);
    case PTREE_FUNCTION_CALL:
        return _function_call(ptree);
    case PTREE_ARGUMENT_LIST:
        return _argument_list(ptree);
    case PTREE_UNARY:
        return _unary(ptree);
    case PTREE_POSTFIX:
        return _postfix(ptree);
    case PTREE_IF:
        return _if(ptree);
    case PTREE_IF_ELSE:
        return _if_else(ptree);
    case PTREE_TERNARY_EXPRESSION:
        return _ternary_expression(ptree);
    case PTREE_ARRAY_LITERAL:
        return _array_literal(ptree);
    case PTREE_ARRAY_ACCESS:
        return _array_access(ptree);
    case PTREE_INTEGER_TYPE:
        return _type_integer(ptree);
    case PTREE_STRING_TYPE:
        return _type_string(ptree);
    case PTREE_ARRAY_TYPE:
        return _type_array(ptree);
    case PTREE_BOOLEAN_TYPE:
        return _type_bool(ptree);
    case PTREE_FUNCTION_TYPE:
        return _type_function(ptree);
    case PTREE_TYPE_REFERENCE:
        return _type_reference(ptree);
    case PTREE_CLASS:
        return _class(ptree);
    case PTREE_STRUCT:
        return _struct(ptree);
    case PTREE_OBJECT_LITERAL:
        return _object_literal(ptree);
    case PTREE_KEYVALUE:
        return _keyvalue(ptree);
    default:
        printf("Unknown type: %d\n", ptree->type);
        break;
    }

    return NULL;
}

void init()
{
    if (strcmp(language_g, "python") == 0)
        phases_g[2] = &MAMBA;
    else if (strcmp(language_g, "js") == 0)
        phases_g[2] = &NaNsense;
    else if (strcmp(language_g, "lugha") == 0)
        phases_g[2] = &LUGHA;
    else if (strcmp(language_g, "c") == 0)
        phases_g[2] = &POSEIDON;
}

void *ast_init(ptree_t *ptree)
{
    if (!ptree)
        return NULL;

    printf("Running phase: Creating Abstract Syntax Tree\n");

    init();

    node_ast_t *ast = gen_ast(ptree);

    for (size_t i = 0; i < array_size(phases_g); i++)
    {
        if (!phases_g[i])
            continue;

        printf("Running phase: %s\n", phases_g[i]->fullname);

        phases_g[i]->init();

        node_ast_t *temp = ast;
        ast->accept(temp, phases_g[i]);

        phases_g[i]->exit(phases_g[i]);
    }

    return (void *)ast;
}

const proc_ast_t node_ast = {
    .fullname = "Default AST",
    .shortname = "d_ast",
    .author = "Kithinji Brian",
    .doc = "Default AST module",
    .init = ast_init,
    .exit = ast_exit,
};
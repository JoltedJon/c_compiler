#ifndef JCC_NODES_H
#define JCC_NODES_H

#include <stdint.h>
#include "src/token.h"

enum node_type {
  // Binary Expressions
  ADD_EXPRESSION,
  SUBTRACT_EXPRESSION,
  MULTIPLY_EXPRESSION,
  DIVIDE_EXPRESSION,
  MODULO_EXPRESSION,
  LOGICAL_AND_EXPRESSION,
  LOGICAL_OR_EXPRESSION,
  EQUALITY_EXPRESSION,
  INEQUALITY_EXPRESSION,
  LESS_EXPRESSION,
  GREATER_EXPRESSION,
  LESS_EQUAL_EXPRESSION,
  GREATER_EQUAL_EXPRESSION,
  LEFT_SHIFT_EXPRESSION,
  RIGHT_SHIFT_EXPRESSION,
  BITWISE_OR_EXPRESSION,
  BITWISE_AND_EXPRESSION,
  BITWISE_XOR_EXPRESSION,
  ASSIGN_EXPRESSION,
  ADD_ASSIGN_EXPRESSION,
  SUBTRACT_ASSIGN_EXPRESSION,
  MULTIPLY_ASSIGN_EXPRESSION,
  DIVIDE_ASSIGN_EXPRESSION,
  MODULO_ASSIGN_EXPRESSION,
  BITWISE_AND_ASSIGN_EXPRESSION,
  BITWISE_OR_ASSIGN_EXPRESSION,
  BITWISE_XOR_ASSIGN_EXPRESSION,
  LEFT_SHIFT_ASSIGN_EXPRESSION,
  RIGHT_SHIFT_ASSIGN_EXPRESSION,
  COMMA_EXPRESSION,

  ARRAY_ACCESS_EXPRESSION,
  FUNCTION_CALL_EXPRESSION,

  // Ternary Expression
  TERNARY_EXPRESSION,

  // Unary Expressions
  POSITIVE_EXPRESSION,
  NEGATION_EXPRESSION,
  LOGICAL_NOT_EXPRESSION,
  BITWISE_NOT_EXPRESSION,
  DEREFERENCE_EXPRESSION,
  ADDRESS_OF_EXPRESSION,
  SIZE_OF_EXPRESSION,
  PREFIX_INCREMENT_EXPRESSION,
  PREFIX_DECREMENT_EXPRESSION,
  POSTFIX_INCREMENT_EXPRESSION,
  POSTFIX_DECREMENT_EXPRESSION,

  DOT_ACCESS_EXPRESSION,
  ARROW_ACCESS_EXPRESSION,

  // Primary expressions
  IDENTIFIER_EXPRESSION,
  STRING_LITERAL_EXPRESSION,
  INTEGER_LITERAL_EXPRESSION,
  FLOAT_LITERAL_EXPRESSION,

  // Misc
  ARGUMENT_LIST,

  // Declarations
  DECLARATION,
  DECL_SPECIFICATION,

  // Statements
  EXPR_STATEMENT,
  LABEL_STATEMENT
};
  
// Base data structure, all types downcast to this and will be upcast based on type 
struct ast_node {
  enum node_type type; 
  int64_t line;
  int64_t column;
};

/* EXPRESSION NODES */

struct binary_expression_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *left;
  struct ast_node *right;
};

struct ternary_expression_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *condition;
  struct ast_node *true_node;
  struct ast_node *false_node;
};

struct unary_expression_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *operand;
};

struct literal_expression_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  union literals lit;
};

struct identifier_expression_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  char *identifier;
};

struct struct_access_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *storage;
  char *identifier;
};

struct argument_list {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node** args;
  int num_args;
};


/* DECLARATION NODES */

struct decl_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *specifiers;
  struct ast_node *init_list;
};

enum decl_spec_type {
  STORAGE_CLASS_SPEC,
  TYPE_SPEC,
  TYPE_QUALIFIER_SPEC,
  FUNCTION_SPEC
};

struct decl_spec_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  enum decl_spec_type spec_type;
  char *spec;
  struct ast_node *more_specs;
};

struct decl_init_list {
  enum node_type type;
  int64_t line;
  int64_t column;

  
};

/* STATEMENT NODES */

struct stmt_expression_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *expr;
};

struct stmt_label_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  char *identifier;
};

struct stmt_case_node {
  enum node_type type;
  int64_t line;
  int64_t column;

  struct ast_node *expr;
  struct ast_node **stmts;
  int64_t num_stmts;
};


void print_node(struct ast_node *node);
void destroy_node(struct ast_node *node);

#endif
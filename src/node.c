#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "nodes.h"

typedef struct ast_node ast_node;

const char* node_strs[] = {
  "+", // "ADD_EXPRESSION",
  "-",//"SUBTRACT_EXPRESSION",
  "*", //"MULTIPLY_EXPRESSION",
  "/", // "DIVIDE_EXPRESSION",
  "%", //"MODULO_EXPRESSION",
  "&&", //"LOGICAL_AND_EXPRESSION",
  "||", //"LOGICAL_OR_EXPRESSION",
  "==", //"EQUALITY_EXPRESSION",
  "!=", //"INEQUALITY_EXPRESSION",
  "<", //"LESS_EXPRESSION",
  ">", //"GREATER_EXPRESSION",
  "<=", //"LESS_EQUAL_EXPRESSION",
  ">=", //"GREATER_EQUAL_EXPRESSION",
  "<<", //"LEFT_SHIFT_EXPRESSION",
  ">>", //"RIGHT_SHIFT_EXPRESSION",
  "|", //"BITWISE_OR_EXPRESSION",
  "&", //"BITWISE_AND_EXPRESSION",
  "^", //"BITWISE_XOR_EXPRESSION",
  "=", //"ASSIGN_EXPRESSION",
  "+=", //"ADD_ASSIGN_EXPRESSION",
  "-=", //"SUBTRACT_ASSIGN_EXPRESSION",
  "*=", //"MULTIPLY_ASSIGN_EXPRESSION",
  "/=", //"DIVIDE_ASSIGN_EXPRESSION",
  "%=", //"MODULO_ASSIGN_EXPRESSION",
  "&=", //"BITWISE_AND_ASSIGN_EXPRESSION",
  "|=", //"BITWISE_OR_ASSIGN_EXPRESSION",
  "^=", //"BITWISE_XOR_ASSIGN_EXPRESSION",
  "<<=", //"LEFT_SHIFT_ASSIGN_EXPRESSION",
  ">>=", //"RIGHT_SHIFT_ASSIGN_EXPRESSION",
  ",", //"COMMA_EXPRESSION",
  "array access", //"ARRAY_ACCESS_EXPRESSION",
  "function call", //"FUNCTION_CALL_EXPRESSION",
  "ternary", //"TERNARY_EXPRESSION",
  "+", //"POSITIVE_EXPRESSION",
  "-", //"NEGATION_EXPRESSION",
  "!", //"LOGICAL_NOT_EXPRESSION",
  "~", //"BITWISE_NOT_EXPRESSION",
  "*", //"DEREFERENCE_EXPRESSION",
  "&", //"ADDRESS_OF_EXPRESSION",
  "sizeof", //"SIZE_OF_EXPRESSION",
  "prefix ++", //"PREFIX_INCREMENT_EXPRESSION",
  "prefix --", //"PREFIX_DECREMENT_EXPRESSION",
  "postfix ++", //"POSTFIX_INCREMENT_EXPRESSION",
  "postfix --", //"POSTFIX_DECREMENT_EXPRESSION",
  ".", //"DOT_ACCESS_EXPRESSION",
  "->", //"ARROW_ACCESS_EXPRESSION",
  "", //"IDENTIFIER_EXPRESSION",
  "", //"STRING_LITERAL_EXPRESSION",
  "", //"INTEGER_LITERAL_EXPRESSION",
  "", //"FLOAT_LITERAL_EXPRESSION",
  "arguments", //"ARGUMENT_LIST"
};

void graph_gen(ast_node *node, const void *parent_ID, const char *connection) {
  if(node == NULL) return;
  {
    char name[512];
    if(node->type == IDENTIFIER_EXPRESSION) {
      struct identifier_expression_node *temp = (struct identifier_expression_node *)node;
      snprintf(name, sizeof(name), "Identifier:\\n%s", temp->identifier);
    }
    else if(node->type == STRING_LITERAL_EXPRESSION) {
      struct literal_expression_node *temp = (struct literal_expression_node *)node;
      snprintf(name, sizeof(name), "String literal:\\n\\\"%s\\\"", temp->lit.str);
    }
    else if(node->type == INTEGER_LITERAL_EXPRESSION) {
      struct literal_expression_node *temp = (struct literal_expression_node *)node;
      snprintf(name, sizeof(name), "Integer literal:\\n%lu", temp->lit.number);
    }
    else if(node->type == FLOAT_LITERAL_EXPRESSION) {
      struct literal_expression_node *temp = (struct literal_expression_node *)node;
      snprintf(name, sizeof(name), "Float literal:\\n%lf", temp->lit.floating_point);
    }
    else {
      snprintf(name, sizeof(name), "%s", node_strs[node->type]);
    }

    if(parent_ID == NULL) {
      printf("digraph {\n  N%p [label=\"%s\"]\n", node, name);
    }
    else {
      printf(" N%p -> {N%p [label=\"%s\"]} [label=\"%s\"]\n", parent_ID, node, name, connection);
    }
  }

  switch(node->type) {
    case ADD_EXPRESSION:
    case SUBTRACT_EXPRESSION:
    case MULTIPLY_EXPRESSION:
    case DIVIDE_EXPRESSION:
    case MODULO_EXPRESSION:
    case LOGICAL_AND_EXPRESSION:
    case LOGICAL_OR_EXPRESSION:
    case EQUALITY_EXPRESSION:
    case INEQUALITY_EXPRESSION:
    case LESS_EXPRESSION:
    case GREATER_EXPRESSION:
    case LESS_EQUAL_EXPRESSION:
    case GREATER_EQUAL_EXPRESSION:
    case LEFT_SHIFT_EXPRESSION:
    case RIGHT_SHIFT_EXPRESSION:
    case BITWISE_OR_EXPRESSION:
    case BITWISE_AND_EXPRESSION:
    case BITWISE_XOR_EXPRESSION:
    case ASSIGN_EXPRESSION:
    case ADD_ASSIGN_EXPRESSION:
    case SUBTRACT_ASSIGN_EXPRESSION:
    case MULTIPLY_ASSIGN_EXPRESSION:
    case DIVIDE_ASSIGN_EXPRESSION:
    case MODULO_ASSIGN_EXPRESSION:
    case BITWISE_AND_ASSIGN_EXPRESSION:
    case BITWISE_OR_ASSIGN_EXPRESSION:
    case BITWISE_XOR_ASSIGN_EXPRESSION:
    case LEFT_SHIFT_ASSIGN_EXPRESSION:
    case RIGHT_SHIFT_ASSIGN_EXPRESSION:
    case COMMA_EXPRESSION:
    {
      struct binary_expression_node *temp_node = (struct binary_expression_node *)node;
      graph_gen(temp_node->left, node, "left");
      graph_gen(temp_node->right, node, "right");
      break;
    }
    case ARRAY_ACCESS_EXPRESSION:
    {
      struct binary_expression_node *temp_node = (struct binary_expression_node *)node;
      graph_gen(temp_node->left, node, "array");
      graph_gen(temp_node->right, node, "index");
      break;
    }
    case FUNCTION_CALL_EXPRESSION: 
    {
      struct binary_expression_node *temp_node = (struct binary_expression_node *)node;
      graph_gen(temp_node->left, node, "function name");
      graph_gen(temp_node->right, node, "args");
      break;
    }
    case TERNARY_EXPRESSION: {
      struct ternary_expression_node *temp_node = (struct ternary_expression_node *)node;
      graph_gen(temp_node->condition, node, "condition");
      graph_gen(temp_node->true_node, node, "true");
      graph_gen(temp_node->false_node, node, "false");
      break;
    }
    case POSITIVE_EXPRESSION:
    case NEGATION_EXPRESSION:
    case LOGICAL_NOT_EXPRESSION:
    case BITWISE_NOT_EXPRESSION:
    case DEREFERENCE_EXPRESSION:
    case ADDRESS_OF_EXPRESSION:
    case SIZE_OF_EXPRESSION:
    case PREFIX_INCREMENT_EXPRESSION:
    case PREFIX_DECREMENT_EXPRESSION:
    case POSTFIX_INCREMENT_EXPRESSION:
    case POSTFIX_DECREMENT_EXPRESSION: 
    case DOT_ACCESS_EXPRESSION:
    case ARROW_ACCESS_EXPRESSION: {
      struct unary_expression_node *temp_node = (struct unary_expression_node *)node;
      graph_gen(temp_node->operand, node, "expr");
      break;
    }
    case ARGUMENT_LIST: {
      struct argument_list *temp_node = (struct argument_list *)node;
      for(int64_t i = 0; i < temp_node->num_args; i++) {
        char buf[50];
        snprintf(buf, sizeof(buf), "arg %ld", i);
        graph_gen(temp_node->args[i], temp_node, buf);
      }
      break;
    }
    default:
      break;
  }

  if(parent_ID == NULL) printf("}\n\n");
}

void print_node(ast_node *node) {
  graph_gen(node, NULL, NULL);
}

void destroy_node(ast_node *node) {
  if(node == NULL) return;
  switch(node->type) {
    case ADD_EXPRESSION:
    case SUBTRACT_EXPRESSION:
    case MULTIPLY_EXPRESSION:
    case DIVIDE_EXPRESSION:
    case MODULO_EXPRESSION:
    case LOGICAL_AND_EXPRESSION:
    case LOGICAL_OR_EXPRESSION:
    case EQUALITY_EXPRESSION:
    case INEQUALITY_EXPRESSION:
    case LESS_EXPRESSION:
    case GREATER_EXPRESSION:
    case LESS_EQUAL_EXPRESSION:
    case GREATER_EQUAL_EXPRESSION:
    case LEFT_SHIFT_EXPRESSION:
    case RIGHT_SHIFT_EXPRESSION:
    case BITWISE_OR_EXPRESSION:
    case BITWISE_AND_EXPRESSION:
    case BITWISE_XOR_EXPRESSION:
    case ASSIGN_EXPRESSION:
    case ADD_ASSIGN_EXPRESSION:
    case SUBTRACT_ASSIGN_EXPRESSION:
    case MULTIPLY_ASSIGN_EXPRESSION:
    case DIVIDE_ASSIGN_EXPRESSION:
    case MODULO_ASSIGN_EXPRESSION:
    case BITWISE_AND_ASSIGN_EXPRESSION:
    case BITWISE_OR_ASSIGN_EXPRESSION:
    case BITWISE_XOR_ASSIGN_EXPRESSION:
    case LEFT_SHIFT_ASSIGN_EXPRESSION:
    case RIGHT_SHIFT_ASSIGN_EXPRESSION:
    case COMMA_EXPRESSION:
    case ARRAY_ACCESS_EXPRESSION:
    case FUNCTION_CALL_EXPRESSION: {
      struct binary_expression_node *temp_node = (struct binary_expression_node *)node;
      destroy_node(temp_node->left);
      destroy_node(temp_node->right);
      free(node);
      break;
    }
    case TERNARY_EXPRESSION: {
      struct ternary_expression_node *temp_node = (struct ternary_expression_node *)node;
      destroy_node(temp_node->condition);
      destroy_node(temp_node->true_node);
      destroy_node(temp_node->false_node);
      free(node);
      break;
    }
    case POSITIVE_EXPRESSION:
    case NEGATION_EXPRESSION:
    case LOGICAL_NOT_EXPRESSION:
    case BITWISE_NOT_EXPRESSION:
    case DEREFERENCE_EXPRESSION:
    case ADDRESS_OF_EXPRESSION:
    case SIZE_OF_EXPRESSION:
    case PREFIX_INCREMENT_EXPRESSION:
    case PREFIX_DECREMENT_EXPRESSION:
    case POSTFIX_INCREMENT_EXPRESSION:
    case POSTFIX_DECREMENT_EXPRESSION: 
    case DOT_ACCESS_EXPRESSION:
    case ARROW_ACCESS_EXPRESSION: {
      struct unary_expression_node *temp_node = (struct unary_expression_node *)node;
      destroy_node(temp_node->operand);
      free(node);
      break;
    }
    case IDENTIFIER_EXPRESSION:
    case STRING_LITERAL_EXPRESSION:
    case INTEGER_LITERAL_EXPRESSION:
    case FLOAT_LITERAL_EXPRESSION:
      free(node);
      break;
    case ARGUMENT_LIST: {
      struct argument_list *temp_node = (struct argument_list *)node;
      for(int i = 0; i < temp_node->num_args; i++) {
        destroy_node(temp_node->args[i]);
      }
      free(temp_node->args);
      free(node);
    }

  }
}
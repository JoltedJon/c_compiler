#include "token.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


struct token* make_token(struct token *t) {
  struct token* tok = malloc(sizeof(struct token));
  memcpy(tok, t, sizeof(struct token));

  return tok;
}

void destroy_token(struct token* tok) {
  if (tok->type == STRING_LITERAL) {
    free(tok->literal.str);
  }
  free(tok->lexeme);
  free(tok);
}


static const char* ks[] = {
    "LEFT_PAREN",
    "RIGHT_PAREN",
    "LEFT_BRACE",
    "RIGHT_BRACE",
    "LEFT_BRACKET",
    "RIGHT_BRACKET",
    "COMMA",
    "DOT",
    "SEMICOLON",
    "TILDE",
    "COLON",
    "QUESTION",
    "BANG",
    "BANG_EQUAL",
    "EQUAL",
    "EQUAL_EQUAL",
    "GREATER",
    "GREATER_EQUAL",
    "SHIFT_RIGHT",
    "SHIFT_RIGHT_EQUAL",
    "LESS",
    "LESS_EQUAL",
    "SHIFT_LEFT",
    "SHIFT_LEFT_EQUAL",
    "PLUS",
    "PLUS_EQUAL",
    "PLUS_PLUS",
    "MINUS",
    "MINUS_EQUAL",
    "MINUS_MINUS",
    "ARROW",
    "STAR",
    "STAR_EQUAL",
    "SLASH",
    "SLASH_EQUAL",
    "PERCENT",
    "PERCENT_EQUAL",
    "AMPERSAND",
    "AMPERSAND_AMPERSAND",
    "AMPERSAND_EQUAL",
    "PIPE",
    "PIPE_PIPE",
    "PIPE_EQUAL",
    "CARET",
    "CARET_EQUAL",
    "IDENTIFIER",
    "STRING_LITERAL",
    "NUMBER_LITERAL",
    "FLOAT_LITERAL",
    "AUTO",
    "BREAK",
    "CASE",
    "CHAR",
    "CONST",
    "CONTINUE",
    "DEFAULT",
    "DO",
    "DOUBLE",
    "ELSE",
    "ENUM",
    "EXTERN",
    "FLOAT",
    "FOR",
    "GOTO",
    "IF",
    "INLINE",
    "INT",
    "LONG",
    "REGISTER",
    "RESTRICT",
    "RETURN",
    "SHORT",
    "SIGNED",
    "SIZEOF",
    "STATIC",
    "STRUCT",
    "SWITCH",
    "TYPEDEF",
    "UNION",
    "UNSIGNED",
    "VOID",
    "VOLATILE",
    "WHILE",
    "EOFF"
};

void print_token(struct token* tok, FILE* fp) { 
  fprintf(fp, "%s %ld:%ld %s ", tok->filename, tok->line, tok->column, ks[tok->type]);

  if(tok->type == STRING_LITERAL) {
    fprintf(fp, "%s ", tok->literal.str);
  } else if(tok->type == NUMBER_LITERAL) {
    fprintf(fp, "%ld ", tok->literal.number);
  } else if(tok->type == FLOAT_LITERAL) {
    fprintf(fp, "%f ", tok->literal.floating_point);
  }

  fprintf(fp, "%s\n", tok->lexeme);
}
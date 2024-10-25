#ifndef JCC_TOKEN_H
#define JCC_TOKEN_H

#include <stdint.h>
#include <stdio.h>

enum token_type {
  // Single-character tokens.
  LEFT_PAREN,     // (
  RIGHT_PAREN,    // )
  LEFT_BRACE,     // {
  RIGHT_BRACE,    // }
  LEFT_BRACKET,   // [
  RIGHT_BRACKET,  // ]
  COMMA,          // ,
  DOT,            // .
  SEMICOLON,      // ;
  TILDE,          // ~
  COLON,          // :
  QUESTION,       // ?

  // One or two character tokens.
  BANG,        // !
  BANG_EQUAL,  // !=

  EQUAL,        // =
  EQUAL_EQUAL,  // ==

  GREATER,            // >
  GREATER_EQUAL,      // >=
  SHIFT_RIGHT,        // >>
  SHIFT_RIGHT_EQUAL,  // >>=

  LESS,              // <
  LESS_EQUAL,        // <=
  SHIFT_LEFT,        // <<
  SHIFT_LEFT_EQUAL,  // <<=

  PLUS,        // +
  PLUS_EQUAL,  // +=
  PLUS_PLUS,   // ++

  MINUS,        // -
  MINUS_EQUAL,  // -=
  MINUS_MINUS,  // --
  ARROW,        // ->

  STAR,        // *
  STAR_EQUAL,  // *=

  SLASH,        // /
  SLASH_EQUAL,  // /=

  PERCENT,        // %
  PERCENT_EQUAL,  // %=

  AMPERSAND,            // &
  AMPERSAND_AMPERSAND,  // &&
  AMPERSAND_EQUAL,      // &=

  PIPE,        // |
  PIPE_PIPE,   // ||
  PIPE_EQUAL,  // |=

  CARET,        // ^
  CARET_EQUAL,  // ^=

  // Literals.
  IDENTIFIER,      // variable names, function names, etc.
  STRING_LITERAL,  // "example"
  NUMBER_LITERAL,  // 123, 'c'
  FLOAT_LITERAL,   // 45.67

  // Keywords.
  AUTO,
  BREAK,
  CASE,
  CHAR,
  CONST,
  CONTINUE,
  DEFAULT,
  DO,
  DOUBLE,
  ELSE,
  ENUM,
  EXTERN,
  FLOAT,
  FOR,
  GOTO,
  IF,
  INLINE,
  INT,
  LONG,
  REGISTER,
  RESTRICT,
  RETURN,
  SHORT,
  SIGNED,
  SIZEOF,
  STATIC,
  STRUCT,
  SWITCH,
  TYPEDEF,
  UNION,
  UNSIGNED,
  VOID,
  VOLATILE,
  WHILE,

  EOFF
};

union literals {
  char* str;
  uint64_t number;
  double floating_point;
};

struct token {
  enum token_type type;
  union literals literal;
  char* lexeme;

  // Error reporting info
  const char *filename;
  uint64_t line;
  uint64_t column;
};

struct token* make_token(struct token *t);

void destroy_token(struct token *tok);

void print_token(struct token *tok, FILE* fp);

#endif
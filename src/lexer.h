#ifndef JCC_LEXER_H
#define JCC_LEXER_H

#include "token.h"

#include <stdbool.h>

struct macro {
  char *label;
  struct token** tokens;
  int tokens_size;
};

struct lexer {
  const char *filename;
  const char *directory;

  char* source;
  int64_t source_length;

  struct token** tokens;
  int64_t tokens_capacity;
  int64_t tokens_size;

  int64_t start;
  int64_t current;
  int64_t line;

  int64_t line_start;

  bool error;
};

struct token** lex_file(const char* filename, int *tokens_size);

#endif
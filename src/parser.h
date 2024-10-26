#ifndef JCC_PARSER_H
#define JCC_PARSER_H

#include <stdint.h>
#include <stdbool.h>

#include "token.h"
#include "nodes.h"

struct parser {
  struct token **tokens;
  int64_t tokens_size;

  int64_t current;

  bool error;
  bool checked_error;
};

struct ast_node *parse(struct token **tokens, int64_t tokens_size);

#endif
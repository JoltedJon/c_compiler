#include "parser.h"
#include "ansi_colors.h"

/* ERROR REPORTING */

static void parser_error(struct parser* p, const char* msg) {
  struct token *t = p->tokens[p->current];

  fprintf(stderr, ANSI_COLOR_RED "Error: " ANSI_COLOR_RESET "%s %ld:%ld %s\n%s\n", 
    t->filename, 
    t->line, 
    t->column, 
    t->lexeme,
    msg
  );

  p->error = true;
}

static void parser_warning(struct parser* p, const char* msg) {
  struct token *t = p->tokens[p->current];

  fprintf(stderr, ANSI_COLOR_YELLOW "Warning: " ANSI_COLOR_RESET  "%s %ld:%ld %s\n%s\n", 
    t->filename, 
    t->line, 
    t->column, 
    t->lexeme,
    msg
  );
  
}

/* END ERROR REPORTING */

struct ast_node *parse(struct token **tokens, int64_t tokens_size) {
  struct parser p = {
    .tokens = tokens,
    .tokens_size = tokens_size,

    .current = 0
  };



  return NULL;
}

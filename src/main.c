#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "token.h"
#include "parser.h"
#include "nodes.h"

void compile(char* filename) {
  int num_tokens;
  struct token** token_list = lex_file(filename, &num_tokens);

  parse(token_list, num_tokens);

  for(int i = 0; i < num_tokens; ++i) {
    // print_token(token_list[i], stdout);
    destroy_token(token_list[i]);
  }
  free(token_list);
}


int main(int argc, char** argv) {
  // TODO option parsing

  if(argc < 2) {
    fprintf(stderr, "Usage: ./jcc [source files]\n");
    return 1;
  }

  for(int i = 1; i < argc; i++) {
    compile(argv[i]);
  }


  return 0;
}
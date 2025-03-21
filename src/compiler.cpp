#include "compiler.hpp"

#include <iostream>

#include "lexer.hpp"
#include "parser.hpp"

namespace JCC {

void Compiler::compile(std::string filename) {
  Lexer lex(filename);

  if (lex.scan()) {
    return;
  }

  Parser p(lex);

  Parser::UniqueNode program = p.parse();
  if (!p.had_error()) {
    p.graph_gen(std::cout, program.get());
  }
}

}  // namespace JCC
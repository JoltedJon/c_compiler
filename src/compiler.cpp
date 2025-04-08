#include "compiler.hpp"

#include <iostream>

#include "lexer.hpp"
#include "parser.hpp"
#include "semantics.hpp"

namespace JCC {

void Compiler::compile(std::string filename) {
  Lexer lex(filename);

  if (lex.scan()) {
    return;
  }

  Parser p(lex);
  UniqueNode program = p.parse();

  SemanticContext::base_types = p.get_base_types();

  program->find_labels(nullptr);
  if (SemanticContext::has_error) {
    return;
  }

  program->resolve_identifiers(nullptr);
  if (SemanticContext::has_error) {
    return;
  }

  program->resolve_types();
  if (SemanticContext::has_error) {
    return;
  }

  p.graph_gen(std::cout, program.get());
  program->constant_fold();
  if (SemanticContext::has_error) {
    return;
  }

  p.graph_gen(std::cout, program.get());
}

}  // namespace JCC
#include "compiler.hpp"

#include <llvm-18/llvm/IR/Value.h>

#include <iostream>

#include "frontend/graph_gen.hpp"
#include "frontend/lexer.hpp"
#include "frontend/parser.hpp"
#include "frontend/semantics.hpp"
#include "src/ast.hpp"

namespace JCC {

void Compiler::compile(std::string filename) {
  UniqueNode program;

  {
    Lexer lex(filename);

    if (lex.scan()) {
      return;
    }

    Parser p(lex);
    program = p.parse();

    SemanticContext::base_types = p.get_base_types();
  }

  program->find_labels(nullptr);
  if (SemanticContext::has_error()) {
    return;
  }

  program->resolve_identifiers(nullptr);
  if (SemanticContext::has_error()) {
    return;
  }

  program->resolve_types();
  if (SemanticContext::has_error()) {
    return;
  }

  program->constant_fold();
  if (SemanticContext::has_error()) {
    return;
  }

  program->control_flow(nullptr);
  if (SemanticContext::has_error()) {
    return;
  }

  if (print_ast_graph) {
    graph_gen(std::cout, program.get());
  }

  std::vector<llvm::Value *> llvm_program = static_cast<TranslationUnit *>(program.get())->codegen();
}

}  // namespace JCC
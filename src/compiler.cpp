#include "compiler.hpp"

#include <llvm-18/llvm/IR/Value.h>
#include <llvm-18/llvm/Support/raw_ostream.h>

#include <iostream>

#include "ast.hpp"
#include "backend/lc2k.hpp"
#include "frontend/graph_gen.hpp"
#include "frontend/lexer.hpp"
#include "frontend/parser.hpp"
#include "frontend/semantics.hpp"
#include "middleend/codegen.hpp"

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
    std::cout << std::flush;
  }

  std::vector<llvm::Value *> llvm_program = static_cast<TranslationUnit *>(program.get())->codegen();

  if (emit_llvm_ir) {
    codegen_context::llvm_module->print(llvm::outs(), nullptr, false, true);
  }

  LC2K::Emitter emitter;

  for (llvm::Function &func : codegen_context::llvm_module->functions()) {
    if (func.isDeclaration()) continue;
    emitter.visitFunction(func);
  }

  emitter.debug_emit(std::cout);
}

}  // namespace JCC
#pragma once

#include <string>

namespace JCC {

struct Compiler {
  bool print_ast_graph = false;
  bool emit_llvm_ir = false;

  std::string output_dir = "";

  void compile(std::string filename);
};

}  // namespace JCC
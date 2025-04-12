#include <iostream>
#include <vector>

#include "compiler.hpp"
#include "include/CLI11.hpp"

void print_usage(char *program_name) {
  std::cout << "usage: " << program_name << " <input_file> [<input_file> ...]" << std::endl;
}

int main(int argc, char **argv) {
  CLI::App app("Jon's C Compiler: Compiles C99 to x86_64, ARM, and even lc2k");
  argv = app.ensure_utf8(argv);

  JCC::Compiler compiler;

  app.add_flag("--emit-llvm", compiler.emit_llvm_ir, "Emits LLVM IR of inputted files")->default_val(false);
  app.add_flag("--ast", compiler.print_ast_graph, "Prints GraphViz compatible graph")->default_val(false);
  app.add_option("--output_dir", compiler.output_dir, "Output Directory to write assembly files to")
      ->default_str("./")
      ->check(CLI::ExistingDirectory);

  std::vector<std::string> source_files;

  app.add_option("input_files", source_files, "Source files to compile")
      ->required()
      ->check(CLI::ExistingFile)
      ->expected(-1);

  CLI11_PARSE(app, argc, argv);

  for (auto &file : source_files) {
    compiler.compile(file);
  }
}
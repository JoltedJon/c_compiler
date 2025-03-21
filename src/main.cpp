#include <iostream>

#include "compiler.hpp"

void print_usage(char *program_name) {
  std::cout << "usage: " << program_name << " <input_file> [<input_file> ...]" << std::endl;
}

int main(int argc, char **argv) {
  if (argc <= 1) {
    print_usage(argv[0]);
    return 1;
  }

  // Lex
  // Preprocess
  // Parse
  // Analyse
  // Generate Code

  for (int i = 1; i < argc; ++i) {
    JCC::Compiler compiler;
    compiler.compile(argv[i]);
  }
}
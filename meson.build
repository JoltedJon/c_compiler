project('jonc', 'cpp', default_options: ['cpp_std=c++23', 'default_library=static'])

cpp_std = 'c++23'
wall = true

add_project_arguments(['-Wall', '-Wextra', '-Wpedantic'], language: 'cpp')

sources = [
  'src/frontend/lexer.cpp',
  'src/frontend/parser.cpp',
  'src/frontend/semantics.cpp',
  'src/frontend/constant_fold.cpp',
  'src/frontend/graph_gen.cpp',
  'src/middleend/codegen.cpp',
  'src/compiler.cpp',
  'src/main.cpp',
]

llvm_dep = dependency('llvm', modules: ['core', 'irreader'], required: true)

executable('jcc', sources, dependencies: [llvm_dep])
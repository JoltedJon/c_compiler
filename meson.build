project('jonc', 'cpp', default_options: ['cpp_std=c++23', 'default_library=static'])

add_project_arguments(['-Wall', '-Wextra'], language: 'c')

sources = [
  'src/lexer.cpp',
  'src/parser.cpp',
  'src/semantics.cpp',
  'src/constant_fold.cpp',
  'src/graph_gen.cpp',
  'src/compiler.cpp',
  'src/main.cpp',
]

llvm_dep = dependency('llvm', modules: ['core', 'irreader'], required: true)

executable('jcc', sources, dependencies: [llvm_dep])
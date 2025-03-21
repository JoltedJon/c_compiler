project('jonc', 'cpp', default_options: ['cpp_std=c++23', 'default_library=static'])

add_project_arguments(['-Wall', '-Wextra'], language: 'c')

sources = [
  'src/lexer.cpp',
  'src/parser.cpp',
  'src/graph_gen.cpp',
  'src/compiler.cpp',
  'src/main.cpp',
]

executable('jcc', sources)
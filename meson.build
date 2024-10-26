project('jonc', 'c', default_options: ['c_std=c11', 'default_library=static'])

add_project_arguments(['-Wall','-Wextra'], language: 'c' )

sources = [
  'src/lexer.c',
  'src/token.c',
  'src/node.c',
  'src/parser.c',
  'src/main.c'
]

executable('jcc', sources)

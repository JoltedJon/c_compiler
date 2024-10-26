#include "lexer.h"
#include "src/ansi_colors.h"
#include "token.h"

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <libgen.h>
#include <time.h>

static void scan_token(struct lexer *l);

struct keyword {
  const char* keyword;
  enum token_type t;
};

#define JCC_NUM_KEYWORDS 34

const struct keyword keywords[] = {
    {"auto", AUTO},         {"break", BREAK},       {"case", CASE},         {"char", CHAR},
    {"const", CONST},       {"continue", CONTINUE}, {"default", DEFAULT},   {"do", DO},
    {"double", DOUBLE},     {"else", ELSE},         {"enum", ENUM},         {"extern", EXTERN},
    {"float", FLOAT},       {"for", FOR},           {"goto", GOTO},         {"if", IF},
    {"inline", INLINE},     {"int", INT},           {"long", LONG},         {"register", REGISTER},
    {"restrict", RESTRICT}, {"return", RETURN},     {"short", SHORT},       {"signed", SIGNED},
    {"sizeof", SIZEOF},     {"static", STATIC},     {"struct", STRUCT},     {"switch", SWITCH},
    {"typedef", TYPEDEF},   {"union", UNION},       {"unsigned", UNSIGNED}, {"void", VOID},
    {"volatile", VOLATILE}, {"while", WHILE}};

// Filename should just be filename with no path information
static const char* add_filename(const char *filename) {
  static int capacity = 1;
  static int size = 0;
  static char** filenames = NULL;
  if(filenames == NULL) {
    filenames = malloc(sizeof(char**) * capacity);
  }

  for(int i = 0; i < size; i++) {
    if(!strcmp(filename, filenames[i])) {
      return filenames[i];
    }
  }

  if(capacity == size) {
    capacity = capacity * 2;
    filenames = realloc(filenames, sizeof(char**) * capacity);
  }

  char *new_filename = malloc(sizeof(char) * (strlen(filename) + 1));
  strcpy(new_filename, filename);

  filenames[size] = new_filename;
  size++;

  return new_filename;
}

static const char* add_directory(const char *directory) {
  static int capacity = 1;
  static int size = 0;
  static char** directories = NULL;
  if(directories == NULL) {
    directories = malloc(sizeof(char**) * capacity);
  }

  for(int i = 0; i < size; i++) {
    if(!strcmp(directory, directories[i])) {
      return directories[i];
    }
  }

  if(capacity == size) {
    capacity = capacity * 2;
    directories = realloc(directories, capacity);
  }

  char *new_directory = malloc(sizeof(char) * (strlen(directory) + 1));
  strcpy(new_directory, directory);

  directories[size++] = new_directory;

  return new_directory;
}

/* ERROR REPORTING */

#define JCC_MAX_COLUMNS 81

static void lexer_error(struct lexer* l, const char* msg) {
  char buff[JCC_MAX_COLUMNS] = {0};
  int i = 0;
  int64_t start_pos = l->line_start;

  // TODO need good checkpoint to not print entire line
  // If long line don't print entire line
  if(l->start - start_pos > JCC_MAX_COLUMNS) {
    start_pos = (l->start - JCC_MAX_COLUMNS) + 10;
  }

  int64_t pos = start_pos;
  while(pos < l->source_length && i < JCC_MAX_COLUMNS && (l->source[pos] != '\n' && l->source[pos] != ';')) {
    buff[i] = l->source[pos];
    i++;
    pos++;
  }

  fprintf(stderr, ANSI_COLOR_RED "Error: " ANSI_COLOR_RESET "%s\n%s %ld:%ld\n%s\n", msg, l->filename, l->line, l->start - l->line_start, buff);

  pos = start_pos;
  while(pos != l->start) {
    fprintf(stderr, "-");
    pos++;
  }


  fprintf(stderr, "^\n\n");
}

static void lexer_warning(struct lexer* l, const char* msg) {
  char buff[JCC_MAX_COLUMNS] = {0};

  int i = 0;
  int pos = l->start;
  while(i < JCC_MAX_COLUMNS && pos < l->source_length && pos < l->current) {
    buff[i] = l->source[pos];
    i++;
    pos++;
  }

  fprintf(stderr, ANSI_COLOR_YELLOW "Warning: " ANSI_COLOR_RESET "%s\n%s %ld:%ld\t%s\n\n", msg, l->filename, l->line, l->start - l->line_start, buff);
}

#undef JCC_MAX_COLUMNS

/* END ERROR REPORTING*/

static inline bool at_end(struct lexer* l) {
  return l->current >= l->source_length;
}

static void append_token(struct lexer *l, struct token *t) {
  if(l->tokens_size == l->tokens_capacity) {
    l->tokens_capacity = l->tokens_capacity * 2;
    l->tokens = realloc(l->tokens, sizeof(struct token*) * l->tokens_capacity);
  }

  l->tokens[l->tokens_size++] = t;
}

static void add_literal_token(struct lexer* l, enum token_type t_type, union literals lit) {
  int size = l->current - l->start;
  char *lexeme = malloc(sizeof(char) * (size + 1));
  strncpy(lexeme, l->source + l->start, size);
  lexeme[size] = '\0';

  struct token t = {
    .type = t_type,
    .literal = lit,
    .lexeme = lexeme,
    .filename = l->filename,
    .line = l->line,
    .column = (l->start - l->line_start)
  };

  append_token(l, make_token(&t));
}

static void add_token(struct lexer* l, enum token_type t_type) {
  union literals lit = { .number = 0};
  add_literal_token(l, t_type, lit);
}

static inline char get(struct lexer* l) {
  return l->source[l->current++];
}

static inline char peek(struct lexer* l) {
  return at_end(l) ? '\0' : l->source[l->current];
}

static char peek_next(struct lexer* l) {
    if (l->current + 1 >= l->source_length) return '\0';
    return l->source[l->current + 1];
  }

static bool match(struct lexer* l, char expected) {
  if(at_end(l) || l->source[l->current] != expected) return false;

  l->current++;
  return true;
}

static void consume_white_space(struct lexer* l, bool inc_line) {
  for(;;) {
    char c = peek(l);
    switch(c) {
      case '\\':
        if(peek_next(l) != '\n') return;
        get(l);
        __attribute__ ((fallthrough)); 
      case '\n':
        if(inc_line) {
          l->line++;
          l->line_start = l->current + 1;
        }
        __attribute__ ((fallthrough));
      case ' ':
      case '\t':
      case '\v':
      case '\f':
      case '\r':
        get(l);
        break;
      default:
        return;
    }

  }
}

static void consume_to_newline(struct lexer *l) {
  while(!at_end(l) && peek(l) != '\n') {
    if(get(l) == '\\' && peek(l) == '\n') {
      l->line++;
      get(l);
    }
  }
}

static inline bool is_octal(char c) {
  return c >= '0' && c < '8';
}

static int hex_value(char c) {
  if(c >= '0' && c <= '9') {
    return c - '0';
  } else if(c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  } else if(c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  }
  return -1;
}

static void read_multiline_comment(struct lexer* l) {
  for(;;) {
    if(match(l, '*') && match(l, '/')) {
      l->line_start = l->current;
      break;
    }
    if(get(l) == '\n') l->line++;

  }
}

static void read_float(struct lexer *l) {
  while(isdigit(peek(l))) get(l);

  if(peek(l) == '.') {
    lexer_error(l, "Invalid floating point number.");
    return;
  }

  // TODO handle scientific notation
  // Currently atof will handle scientific notation but lexer does not

  union literals lit;
  lit.floating_point = atof(l->source + l->start);

  if(errno == ERANGE) {
    lexer_warning(l, "Floating point number overflow.");
  }

  add_literal_token(l, FLOAT_LITERAL, lit);
}

static void read_number(struct lexer *l) {
  // TODO handle different bases 
  // has to start with '0' then specifier
  while(isdigit(peek(l))) get(l);

  if(peek(l) == '.') {
    get(l);
    read_float(l);
    return;
  }

  union literals lit;
  char* endptr;
  lit.number = strtoll(l->source + l->start, &endptr, 10);

  if(errno == ERANGE) {
    lexer_warning(l, "Integer value overflow");
  }

  add_literal_token(l, NUMBER_LITERAL, lit);
}

static char read_octal(struct lexer *l) {
  unsigned int val = 0;
  for(int i = 0; i < 3; i++) {
    char c = peek(l);
    if(!is_octal(c)) break;
    val = val << 3;
    c = get(l) - '0';
    val = val | c;
  }

  if(val > 255) {
    lexer_error(l, "Octal escape sequence out of range");
  }

  return val;
}

static char read_hex(struct lexer *l) {
  unsigned char c = 0;
  for(int i = 0; i < 2; i++) {
    int val = hex_value(peek(l));
    if(val == -1) break;
    c = c << 4;
    get(l);
    c = c | (unsigned char)val;
  }

  if(hex_value(peek(l)) != -1) {
    lexer_warning(l, "Hex values longer than two digits not supported.");
  }

  return c;
}

static char read_escape(struct lexer *l) {
  if(at_end(l)) return '\0';
  char c = peek(l);
  switch(c) {
    case 'a':
      c = 0x07;
      get(l);
      break;
    case 'b':
      c = 0x08;
      get(l);
      break;
    case 'f':
      c = 0x0C;
      get(l);
      break;
    case 'n':
      c = 0x0A;
      get(l);
      break;
    case 'r':
      c = 0x0D;
      get(l);
      break;
    case 't':
      c = 0x09;
      get(l);
      break;
    case 'v':
      c = 0x0B;
      get(l);
      break;
    case '\\':
      c = '\\';
      get(l);
      break;
    case '\'':
      c = '\'';
      get(l);
      break;
    case '"':
      c = '"';
      get(l);
      break;
    case '?':
      c = '?';
      get(l);
      break;
    case 'x':
      c = read_hex(l);
      break;
    default:
      if (is_octal(c)) c = read_octal(l);
      else lexer_warning(l, "Unknown escape sequence.");
      // Simply ignore escape sequence if not valid
      break;
  }

  return c;
}

static void read_string(struct lexer *l) {
  int64_t line = l->line;
  int64_t column = l->line_start;
  uint64_t size = 0;
  for(;;) {
    // Keep reading in string until there aren't anymore to concatenate together
    for(char c = peek(l); c != '"' && !at_end(l); c = peek(l)) {
      if(c == '\\') {
        get(l);
        (void)read_escape(l);
      } else if(c == '\n') {
        l->line++;
        l->line_start++;
      } else {
        get(l);
      }
      size++;
    }

    if(at_end(l)) {
      lexer_error(l, "Unterminated string literal.");
      return;
    }

    get(l);

    consume_white_space(l, true);

    if(peek(l) != '"') {
      break;
    }
    get(l);
  }

  char *str = malloc(sizeof(char) * (size + 1));
  str[size] = '\0';

  l->current = l->start + 1;

  uint64_t pos = 0;
  for(;;) {
    // Keep reading in string until there aren't anymore to concatenate together
    for(char c = peek(l); c != '"' && !at_end(l); c = peek(l)) {
      if(c == '\\') {
        get(l);
        str[pos] = read_escape(l);
      } else {
        str[pos] = get(l);
      }
      pos++;
    }

    get(l);

    consume_white_space(l, false);

    if(peek(l) != '"') {
      break;
    }
    get(l);
  }

  union literals lit = { .str = str };

  int64_t saved_line = l->line;
  int64_t saved_column = l->line_start;

  l->line = line;
  l->line_start = column;

  add_literal_token(l, STRING_LITERAL, lit);

  l->line = saved_line;
  l->line_start = saved_column;
}

static void read_char(struct lexer *l) {
  if(at_end(l)) {
    lexer_error(l, "Unterminated character literal.");
    return;
  }

  unsigned char c;
  if(peek(l) == '\\') {
    get(l);
    c = read_escape(l);
  } else {
    c = get(l);
  }

  if(get(l) != '\'' || at_end(l)) {
    lexer_error(l, "Unterminated character literal.");
    return;
  }

  union literals lit = { .number = c};
  add_literal_token(l, NUMBER_LITERAL, lit);
}

static void read_identifier(struct lexer *l) {
  while(isalnum(peek(l)) || peek(l) == '_') get(l);

  uint64_t size = l->current - l->start;
  char *temp_str = malloc(sizeof(char) * (size + 1));
  temp_str[size] = '\0';
  strncpy(temp_str, l->source + l->start, size);

  enum token_type t = IDENTIFIER;

  for(int i = 0; i < JCC_NUM_KEYWORDS; i++) {
    if(!strcmp(temp_str, keywords[i].keyword)) {
      t = keywords[i].t;
      break;
    }
  }

  // Predefined macros
  if(!strcmp("__LINE__", temp_str)) {
    union literals lit = {.number = l->line };
    add_literal_token(l, NUMBER_LITERAL, lit);
  } 
  else if(!strcmp("__FILE__", temp_str)) {
    char *str = malloc(strlen(l->filename) + 1);
    strcpy(str, l->filename);
    union literals lit = {.str = str };
    add_literal_token(l, STRING_LITERAL, lit);
  }
  else if(!strcmp("__DATE__", temp_str)) {
    const char* date = __DATE__;
    char * str = malloc(strlen(date) + 1);
    strcpy(str, date);
    union literals lit = { . str = str };
    add_literal_token(l, STRING_LITERAL, lit);
  }
  else if(!strcmp("__TIME__", temp_str)) {
    const char* time = __TIME__;
    char * str = malloc(strlen(time) + 1);
    strcpy(str, time);
    union literals lit = { . str = str };
    add_literal_token(l, STRING_LITERAL, lit);
  }
  else {
    add_token(l, t);
  }

  free(temp_str);
}

static void macro_include(struct lexer *l) {
  while(peek(l) != '\n' && isspace(peek(l))) {
    get(l);
  }

  bool relative = false;
  if(peek(l) == '"') {
    relative = true;
  }
  else if(peek(l) == '<') {
    relative = false;
  }
  else {
    lexer_error(l, "Unrecognized #include directive.");
    consume_to_newline(l);
    return;
  }

  get(l);

  l->start = l->current;

  int size = 0;
  char end_char = relative ? '"' : '>';

  while(peek(l) != end_char) {
    if(at_end(l)) {
      lexer_error(l, "Unterminated include directive.");
      return;
    }
    size++;
    get(l);
  }

  char *include_file = malloc(size + 1);
  strncpy(include_file, l->source + l->start, size);
  include_file[size] = '\0';

  get(l);

  // printf("%s\n", include_file);

  if(relative) {
    size = size + strlen(l->directory) + 1;
    char *filename = malloc(size + 1);
    strcpy(filename, l->directory);
    strcat(filename, "/");
    strcat(filename, include_file);

    int token_size;
    struct token** tokens = lex_file(filename, &token_size);

    for(int i = 0; i < token_size; i++) {
      if(tokens[i]->type == EOFF) continue;
      append_token(l, tokens[i]);
    }

    free(tokens);
    free(filename);
  }
  else {
    lexer_error(l, "Includes of standard library not yet supported.");
  }

  free(include_file);
}

static void macro_define(struct lexer *l) {
  // TODO currently function macros not supported
  lexer_error(l, "define not yet implemented.");

  // while(!at_end(l) && isspace(peek(l))) {
  //   get(l);
  // }

  // l->start = l->current;

  // while(!at_end(l) && !isspace(peek(l))) {
  //   get(l);
  // }

  // int size = l->current - l->start;

  // char *str = malloc(size + 1);
  // strncpy(str, l->source + l->start, size);
  // str[size] = '\0';

  // printf("%s\n", str);

  // l->start = l->current;
  // consume_to_newline(l);

  // size = l->current - l->start;

  // char *temp_src = malloc(size + 1);
  // strncpy(temp_src, l->source + l->start, size);
  // temp_src[size] = '\0';

  // struct token **temp_tokens = malloc(sizeof(struct token**));

  // struct lexer temp_l = {
  //   .filename = l->filename,
  //   .directory = l->directory,
  //   .source = temp_src,
  //   .source_length = size,
  //   .tokens = temp_tokens, 
  //   .tokens_capacity = 1, 
  //   .tokens_size =  0, 
  //   .start = 0, 
  //   .current = 0, 
  //   .line = l->line,
  //   .line_start = 0,
  //   .error = false
  // };

  // while(!at_end(&temp_l)) {
  //   scan_token(&temp_l);
  // }
  // free(temp_src);
  
  // if(temp_l.tokens_size == 0) {
  //   free(temp_tokens);
  // } else {

  // }

  // free(str);
}

static void macro_error(struct lexer *l) {
  l->start = l->current;
  consume_to_newline(l);

  int size = l->current - l->start;
  char *temp_str = malloc(size + 1);
  strncpy(temp_str, l->source + l->start, size);
  temp_str[size] = '\0';

  lexer_error(l, temp_str);

  free(temp_str);
}

static void read_macro(struct lexer *l) {
  // Consume any spaces after #
  while(!at_end(l) && isspace(peek(l))) {
    get(l);
  }

  l->start = l->current;

  while(!at_end(l) && !isspace(peek(l))) {
    get(l);
  }

  int size = l->current - l->start;
  char *str = malloc(size + 1);
  strncpy(str, l->source + l->start, size);
  str[size] = '\0';

  // printf("%s\n", str);

  if(!strcmp("include", str)) {
    macro_include(l);
  }
  else if(!strcmp("define", str)) {
    macro_define(l);
    consume_to_newline(l);
  }
  else if(!strcmp("undef", str)) {
    // TODO undef
    lexer_error(l, "#undef not yet implemented");
    consume_to_newline(l);
  }
  else if(!strcmp("error", str)) {
    macro_error(l);
  }
  else if(!strcmp("pragma", str)) {
    // TODO pragma
    lexer_error(l, "#pragma not yet implemented");
    consume_to_newline(l);
  }
  else if(!strcmp("if", str)) {
    lexer_error(l, "#if not yet implemented");
    // TODO if
    consume_to_newline(l);
  }
  else if(!strcmp("ifdef", str)) {
    lexer_error(l, "#ifdef not yet implemented");
    // TODO ifdef
    consume_to_newline(l);
  }
  else if(!strcmp("ifndef", str)) {
    lexer_error(l, "#ifndef not yet implemented");
    // TODO ifndef
    consume_to_newline(l);
  }
  else if(!strcmp("elif", str)) {
    lexer_error(l, "#elif not yet implemented");
    // TODO elif
    consume_to_newline(l);
  }
  else if(!strcmp("else", str)) {
    lexer_error(l, "#else not yet implemented");
    // TODO else
    consume_to_newline(l);
  }
  else if(!strcmp("endif", str)) {
    lexer_error(l, "#endif not yet implemented");
    // TODO endif
    consume_to_newline(l);
  }
  else {
    lexer_error(l, "Unknown preprocessing directive.");

    consume_to_newline(l);
  }

  free(str);
}

static void scan_token(struct lexer *l) {
  char c = get(l);
  switch(c) {
    case '(':
      add_token(l, LEFT_PAREN);
      break;
    case ')':
      add_token(l, RIGHT_PAREN);
      break;
    case '{':
      add_token(l, LEFT_BRACE);
      break;
    case '}':
      add_token(l, RIGHT_BRACE);
      break;
    case '[':
      add_token(l, LEFT_BRACKET);
      break;
    case ']':
      add_token(l, RIGHT_BRACKET);
      break;
    case ',':
      add_token(l, COMMA);
      break;
    case '.':
      if (isdigit(peek(l))) {
        read_float(l);
      } else {
        add_token(l, DOT);
      }
      break;
    case ';':
      add_token(l, SEMICOLON);
      break;
    case '~':
      add_token(l, TILDE);
      break;
    case ':':
      add_token(l, COLON);
      break;
    case '?':
      add_token(l, QUESTION);
      break;

    // One or more
    case '!':
      add_token(l, match(l, '=') ? BANG_EQUAL : BANG);
      break;
    case '=':
      add_token(l, match(l, '=') ? EQUAL_EQUAL : EQUAL);
      break;
    case '>':
      if (match(l, '=')) {
        add_token(l, GREATER_EQUAL);
      } else if (match(l, '>')) {
        add_token(l, match(l, '=') ? SHIFT_RIGHT_EQUAL
                            : SHIFT_RIGHT);
      } else {
        add_token(l, GREATER);
      }
      break;
    case '<':
      if (match(l, '=')) {
        add_token(l, LESS_EQUAL);
      } else if (match(l, '<')) {
        add_token(l, match(l, '=') ? SHIFT_LEFT_EQUAL
                            : SHIFT_LEFT);
      } else {
        add_token(l, LESS);
      }
      break;
    case '+':
      if (match(l, '=')) {
        add_token(l, PLUS_EQUAL);
      } else if (match(l, '+')) {
        add_token(l, PLUS_PLUS);
      } else {
        add_token(l, PLUS);
      }
      break;
    case '-':
      if (match(l, '=')) {
        add_token(l, MINUS_EQUAL);
      } else if (match(l, '-')) {
        add_token(l, MINUS_MINUS);
      } else if (match(l, '>')) {
        add_token(l, ARROW);
      } else {
        add_token(l, MINUS);
      }
      break;
    case '*':
      add_token(l, match(l, '=') ? STAR_EQUAL : STAR);
      break;
    case '/':
      if (match(l, '/')) {
        // comment
        while (peek(l) != '\n' && !at_end(l)) get(l);
      } else if (match(l, '*')) {
        read_multiline_comment(l);
      } else {
        add_token(l, match(l, '=') ? SLASH_EQUAL : SLASH);
      }
      break;
    case '%':
      add_token(l, match(l, '=') ? PERCENT_EQUAL : PERCENT);
      break;
    case '&':
      if (match(l, '=')) {
        add_token(l, AMPERSAND_EQUAL);
      } else if (match(l, '&')) {
        add_token(l, AMPERSAND_AMPERSAND);
      } else {
        add_token(l, AMPERSAND);
      }
      break;
    case '|':
      if (match(l, '=')) {
        add_token(l, PIPE_EQUAL);
      } else if (match(l, '|')) {
        add_token(l, PIPE_PIPE);
      } else {
        add_token(l, PIPE);
      }
      break;
    case '^':
      add_token(l, match(l, '=') ? CARET_EQUAL : CARET);
      break;

    // Whitespace
    case '\n':
      l->line++;
      l->line_start = l->current;
    case ' ':
    case '\r':
    case '\t':
      break;
    case '"':
      read_string(l);
      break;
    case '\'':
      read_char(l);
      break;
    case '#':
      read_macro(l);
      break;
    default:
      if (isdigit(c)) {
        read_number(l);
      } else if (isalpha(c) || c == '_') {
        read_identifier(l);
      } else {
        lexer_error(l, "Unexpected Character");
      }
      break;
  }
}

struct token** lex_file(const char* filename, int *tokens_size) {
  struct lexer l = {
    .filename = NULL,
    .directory = NULL,
    .source = NULL, 
    .tokens = NULL, 
    .tokens_capacity = 1, 
    .tokens_size =  0, 
    .start = 0, 
    .current = 0, 
    .line = 1,
    .line_start = 0,
    .error = false
  };
  FILE *file = fopen(filename, "rb");

  if(file == NULL) {
    char buffer[512];
    snprintf(buffer, 512, "Lexer - failed to read \"%s\"", filename);
    perror(buffer);
    return NULL;
  }

  if(fseek(file, 0, SEEK_END) < 0) {
   fseek_fail:
    perror("Lexer - fseek() failed");
    fclose(file);
    return NULL;
  }

  if((l.source_length = ftell(file)) < 0) {
    perror("Lexer - ftell() failed");
    fclose(file);
    return NULL;
  }

  if(fseek(file, 0, SEEK_SET) < 0) goto fseek_fail;

  if((l.source = malloc(sizeof(char) * (l.source_length + 1))) == NULL) {
    fprintf(stderr, "Lexer - malloc failed\n");
    return NULL;
  }

  fread(l.source, 1, l.source_length, file);
  l.source[l.source_length] = '\0';

  l.tokens = malloc(sizeof(struct token*) * l.tokens_capacity);

  fclose(file);

  char *temp_filename = malloc(sizeof(char) * (strlen(filename) + 1));
  strcpy(temp_filename, filename);

  l.filename = add_filename(basename(temp_filename));

  strcpy(temp_filename, filename);
  l.directory = add_directory(dirname(temp_filename));

  free(temp_filename);

  while(!at_end(&l)) {
    scan_token(&l);
    l.start = l.current;
  }

  add_token(&l, EOFF);
  free(l.source);

  if(l.error) {
    for(int i = 0; i < l.tokens_size; i++) {
      destroy_token(l.tokens[i]);
    }
    free(l.tokens);
    return NULL;
  }


  *tokens_size = l.tokens_size;
  return l.tokens;
}
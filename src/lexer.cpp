#include "lexer.hpp"

#include <cctype>
#include <ctime>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "ansi_colors.hpp"

namespace JCC {

Lexer::Lexer(const std::string &p_filename)
    : m_filename(p_filename),
      m_start(0),
      m_current(0),
      m_line(1),
      m_column_start(1),
      m_column_end(1),
      m_token_index(0),
      m_error(false),
      m_macros(nullptr) {
  std::ifstream file(p_filename);
  std::stringstream buffer;
  buffer << file.rdbuf();

  m_source = buffer.str();
}

Lexer::Lexer(const std::string &p_filename,
             std::shared_ptr<std::unordered_map<std::string, std::pair<int, MacroFuncPtr>>> p_macros)
    : Lexer(p_filename) {
  m_macros = p_macros;
}

bool Lexer::scan() {
  // Tokenize Input
  while (!at_end()) {
    scan_token();
    m_start = m_current;
    m_column_start = m_column_end;
  }

  [[unlikely]]
  if (m_error) {
    return false;
  }

  preprocess();
  return m_error;
}

void Lexer::error(const std::string p_message) {
  // TODO better error reporting
  m_error = true;

  std::cerr << ANSI_COLOR_RED "Error: " ANSI_COLOR_RESET << m_filename << ":" << m_line << ":" << m_column_start << "-"
            << "\t\"" << m_source.substr(m_start, m_current - m_start) << "\"\n"
            << p_message << std::endl;
}

char Lexer::advance() {
  [[unlikely]]
  if (at_end()) {
    return '\0';
  }

  m_column_end++;
  if (m_source[m_current] == '\n') {
    m_line++;
    m_column_end = 1;
  }

  return m_source[m_current++];
}

bool Lexer::match(char p_c) {
  if (peek() == p_c) {
    advance();
    return true;
  }
  return false;
}

void Lexer::scan_token() {
  char c = advance();
  switch (c) {
    case '(':
      make_token(Token::Type::OPEN_PAREN);
      break;
    case ')':
      make_token(Token::Type::CLOSE_PAREN);
      break;
    case '{':
      make_token(Token::Type::OPEN_CURLY_BRACE);
      break;
    case '}':
      make_token(Token::Type::CLOSE_CURLY_BRACE);
      break;
    case '[':
      make_token(Token::Type::OPEN_SQUARE_BRACKET);
      break;
    case ']':
      make_token(Token::Type::CLOSE_SQUARE_BRACKET);
      break;
    case ',':
      make_token(Token::Type::COMMA);
      break;
    case ';':
      make_token(Token::Type::SEMICOLON);
      break;
    case '~':
      make_token(Token::Type::TILDE);
      break;
    case ':':
      make_token(Token::Type::COLON);
      break;
    case '?':
      make_token(Token::Type::QUESTION);
      break;

    // One or more
    case '.':
      if (isdigit(peek())) {
        read_number();
      }
      else if (peek() == '.') {
        if (peek() == '.') {
          make_token(Token::Type::ELLIPSIS);
        }
        else {
          error("Unexpected Character");
        }
      }
      else {
        make_token(Token::Type::DOT);
      }
      break;
    case '!':
      make_token(match('=') ? Token::Type::BANG_EQUAL : Token::Type::BANG);
      break;
    case '=':
      make_token(match('=') ? Token::Type::EQUAL_EQUAL : Token::Type::EQUAL);
      break;
    case '>':
      if (match('=')) {
        make_token(Token::Type::GREATER_EQUAL);
      }
      else if (match('>')) {
        make_token(match('=') ? Token::Type::SHIFT_RIGHT_EQUAL : Token::Type::SHIFT_RIGHT);
      }
      else {
        make_token(Token::Type::GREATER);
      }
      break;
    case '<':
      if (match('=')) {
        make_token(Token::Type::LESS_EQUAL);
      }
      else if (match('<')) {
        make_token(match('=') ? Token::Type::SHIFT_LEFT_EQUAL : Token::Type::SHIFT_LEFT);
      }
      else {
        make_token(Token::Type::LESS);
      }
      break;
    case '+':
      if (match('=')) {
        make_token(Token::Type::PLUS_EQUAL);
      }
      else if (match('+')) {
        make_token(Token::Type::PLUS_PLUS);
      }
      else {
        make_token(Token::Type::PLUS);
      }
      break;
    case '-':
      if (match('=')) {
        make_token(Token::Type::MINUS_EQUAL);
      }
      else if (match('-')) {
        make_token(Token::Type::MINUS_MINUS);
      }
      else if (match('>')) {
        make_token(Token::Type::ARROW);
      }
      else {
        make_token(Token::Type::MINUS);
      }
      break;
    case '*':
      make_token(match('=') ? Token::Type::STAR_EQUAL : Token::Type::STAR);
      break;
    case '/':
      if (match('/')) {
        // comment
        while (peek() != '\n' && !at_end()) advance();
      }
      else if (match('*')) {
        read_multiline_comment();
      }
      else {
        make_token(match('=') ? Token::Type::SLASH_EQUAL : Token::Type::SLASH);
      }
      break;
    case '%':
      make_token(match('=') ? Token::Type::PERCENT_EQUAL : Token::Type::PERCENT);
      break;
    case '&':
      if (match('=')) {
        make_token(Token::Type::AMPERSAND_EQUAL);
      }
      else if (match('&')) {
        make_token(Token::Type::AMPERSAND_AMPERSAND);
      }
      else {
        make_token(Token::Type::AMPERSAND);
      }
      break;
    case '|':
      if (match('=')) {
        make_token(Token::Type::PIPE_EQUAL);
      }
      else if (match('|')) {
        make_token(Token::Type::PIPE_PIPE);
      }
      else {
        make_token(Token::Type::PIPE);
      }
      break;
    case '^':
      make_token(match('=') ? Token::Type::CARET_EQUAL : Token::Type::CARET);
      break;

    // Whitespace
    case '\\':
      if (peek() == '\n') {
        advance();
        break;
      }
      error("Expected Expression");
      break;
    case '\n':
      make_token(Token::Type::NEWLINE);
      [[fallthrough]];
    case ' ':
    case '\r':
    case '\t':
      while (peek() == '\n' || peek() == ' ' || peek() == '\r' || peek() == '\t') {
        advance();
      }
      break;
    case '"':
      read_string();
      break;
    case '\'':
      read_char();
      break;
    case 'L':
      if (peek() == '\'') {
        read_char();
      }
      else {
        read_identifier();
      }
    case '#':
      read_macro();
      break;
    default:
      if (isdigit(c)) {
        read_number();
      }
      else if (isalpha(c) || c == '_') {
        read_identifier();
      }
      else {
        error("Unexpected Character");
      }
      break;
  }
}

void Lexer::make_token(Token::Type p_type) { make_token(p_type, 0ul); }
void Lexer::make_token(Token::Type p_type, LiteralType p_literal) {
  int source_length = m_current - m_start;
  m_tokens.emplace_back(m_filename, m_line, m_column_start, m_start, p_type, m_source.substr(m_start, source_length),
                        p_literal);
}

void Lexer::read_char() {
  [[unlikely]]
  if (at_end()) {
    error("Unterminated character literal.");
    return;
  }

  char c;
  if (peek() == '\\') {
    advance();
    c = read_escape();
  }
  else {
    c = advance();
  }

  if (advance() != '\'') {
    error("Unterminated character literal.");
    return;
  }

  make_token(Token::Type::NUMBER_LITERAL, static_cast<uint64_t>(c));
}

void Lexer::read_string() {
  std::string str;

  while (!at_end() && peek() != '"') {
    char c = advance();
    if (c == '\\') {
      c = read_escape();
    }
    str += c;
  }

  if (peek() != '"') {
    error("Unterminated string literal.");
    return;
  }

  advance();

  make_token(Token::Type::STRING_LITERAL, str);
}

void Lexer::read_identifier() {
  static std::unordered_map<std::string, Token::Type> identifiers = {
      {"auto", Token::Type::AUTO},         {"break", Token::Type::BREAK},
      {"case", Token::Type::CASE},         {"char", Token::Type::CHAR},
      {"const", Token::Type::CONST},       {"continue", Token::Type::CONTINUE},
      {"default", Token::Type::DEFAULT},   {"do", Token::Type::DO},
      {"double", Token::Type::DOUBLE},     {"else", Token::Type::ELSE},
      {"enum", Token::Type::ENUM},         {"extern", Token::Type::EXTERN},
      {"float", Token::Type::FLOAT},       {"for", Token::Type::FOR},
      {"goto", Token::Type::GOTO},         {"if", Token::Type::IF},
      {"inline", Token::Type::INLINE},     {"int", Token::Type::INT},
      {"long", Token::Type::LONG},         {"return", Token::Type::RETURN},
      {"short", Token::Type::SHORT},       {"signed", Token::Type::SIGNED},
      {"sizeof", Token::Type::SIZEOF},     {"static", Token::Type::STATIC},
      {"struct", Token::Type::STRUCT},     {"switch", Token::Type::SWITCH},
      {"typedef", Token::Type::TYPEDEF},   {"union", Token::Type::UNION},
      {"unsigned", Token::Type::UNSIGNED}, {"void", Token::Type::VOID},
      {"volatile", Token::Type::VOLATILE}, {"while", Token::Type::WHILE}};

  static std::unordered_map<int, std::string> months = {
      {0, "January"}, {1, "February"}, {2, "March"},     {3, "April"},   {4, "May"},       {5, "June"},
      {6, "July"},    {7, "August"},   {8, "September"}, {9, "October"}, {10, "November"}, {11, "December"}};

  while (std::isalnum(peek()) || peek() == '_') advance();

  std::string identifier = m_source.substr(m_start, m_current - m_start);
  auto it = identifiers.find(identifier);
  if (it != identifiers.end()) {
    make_token(it->second);
    return;
  }
  else if (identifier == "__LINE__") {
    make_token(Token::Type::NUMBER_LITERAL, static_cast<uint64_t>(m_line));
  }
  else if (identifier == "__FILE__") {
    make_token(Token::Type::STRING_LITERAL, m_filename);
  }
  else if (identifier == "__DATE__") {
    struct tm *datetime;
    time_t rawtime;

    time(&rawtime);
    datetime = localtime(&rawtime);

    std::stringstream ss;
    ss << months[datetime->tm_mon] << " " << datetime->tm_mday << " " << datetime->tm_year + 1900;

    make_token(Token::Type::STRING_LITERAL, ss.str());
  }
  else if (identifier == "__TIME__") {
    struct tm *datetime;
    time_t rawtime;

    time(&rawtime);
    datetime = localtime(&rawtime);

    std::stringstream ss;
    ss << datetime->tm_hour << ":" << datetime->tm_min << ":" << datetime->tm_sec;
    make_token(Token::Type::STRING_LITERAL, ss.str());
  }
  else {
    make_token(Token::Type::IDENTIFIER, identifier);
  }
}

void Lexer::read_macro() {
  static std::unordered_map<std::string, Token::Type> macros = {
      {"#define", Token::Type::DEFINE},    {"#undef", Token::Type::UNDEFINE},
      {"#ifdef", Token::Type::IF_DEFINED}, {"#ifndef", Token::Type::IF_NOT_DEFINED},
      {"#endif", Token::Type::END_IF},     {"#include", Token::Type::INCLUDE},
      {"#error", Token::Type::ERROR}};

  while (std::isalpha(peek())) advance();

  std::string directive = m_source.substr(m_start, m_current - m_start);
  auto it = macros.find(directive);

  [[unlikely]]
  if (it == macros.end()) {
    error("Invalid preprocessing directive");
    return;
  }

  make_token(it->second);
}

void Lexer::read_multiline_comment() {
  while (!at_end()) {
    if (match('*') && match('/')) break;
  }
}

constexpr bool is_digit(char p_c) { return p_c >= '0' && p_c <= '9'; }
constexpr bool is_hex_digit(char p_c) {
  return is_digit(p_c) || (p_c >= 'a' && p_c <= 'f') || (p_c >= 'A' && p_c <= 'F');
}
constexpr bool is_octal_digit(char p_c) { return p_c >= '0' && p_c <= '7'; }
constexpr bool is_binary_digit(char p_c) { return p_c == '1' || p_c == '0'; }

constexpr int hex_value(char p_c) {
  if (is_digit(p_c)) {
    return p_c - '0';
  }
  else if (p_c >= 'A' && p_c <= 'F') {
    return p_c - 'A' + 10;
  }
  else if (p_c >= 'a' && p_c <= 'f') {
    return p_c - 'a' + 10;
  }
  return -1;
}

Token::NumType Lexer::read_integer_suffix() {
  static std::unordered_map<std::string, Token::NumType> n_types = {
      {"u", Token::NumType::UNSIGNED_INTEGER},  {"l", Token::NumType::SIGNED_INTEGER},
      {"ul", Token::NumType::UNSIGNED_INTEGER}, {"lu", Token::NumType::UNSIGNED_INTEGER},
      {"ull", Token::NumType::UNSIGNED_LONG},   {"llu", Token::NumType::UNSIGNED_LONG},
      {"ll", Token::NumType::SIGNED_LONG}};

  std::string suffix = "";

  while (std::isalpha(peek())) {
    suffix += tolower(advance());
  }

  auto it = n_types.find(suffix);
  if (it != n_types.end()) {
    return it->second;
  }

  error(std::format(R"(Invalid suffix "{}" on integer constant)", suffix));
  return Token::NumType::NONE;
}

void Lexer::read_number() {
  int base = 10;
  bool has_decimal = false;
  bool has_exponent = false;
  bool need_digits = false;
  bool has_error = false;
  bool (*digit_check_func)(char) = is_digit;
  Token::NumType n_type = Token::NumType::SIGNED_INTEGER;

  if (peek() == '0') {
    advance();
  }

  if (peek(-1) == '.') {
    has_decimal = true;
  }
  else if (peek(-1) == '0') {
    if (peek() == 'x') {
      base = 16;
      digit_check_func = is_hex_digit;
      need_digits = true;
      advance();
    }
    else if (peek() == 'b') {
      base = 2;
      digit_check_func = is_binary_digit;
      need_digits = true;
      advance();
    }
  }

  while (digit_check_func(peek())) {
    need_digits = false;
    advance();
  }

  if (peek() == '.') {
    if (base == 10 && !has_decimal) {
      has_decimal = true;
    }
    else {
      error("Invalid use of decimal point");
      has_error = true;
    }

    if (!has_error) {
      advance();
      while (digit_check_func(peek())) {
        advance();
      }
    }
  }

  if (base == 10) {
    if (peek() == 'e' || peek() == 'E') {
      has_exponent = true;
      advance();

      // e+10 e-10
      if (peek() == '+' || peek() == '-') {
        advance();
      }

      // Exponents can only be base 10
      if (!is_digit(peek())) {
        error(R"(Expected exponent value after "e".)");
        has_error = true;
      }

      while (is_digit(peek())) {
        advance();
      }
    }
    else if (std::isalpha(peek())) {
      n_type = read_integer_suffix();
    }
  }

  if (need_digits) {
    error("Expected digits");
    return;
  }

  if (!has_error && has_decimal && peek() == '.') {
    error("Number cannot contain two '.'");
  }

  int len = m_current - m_start;
  std::string number_str = m_source.substr(m_start, len);

  if (!(has_decimal || has_exponent)) {
    LiteralType lit;
    switch (n_type) {
      case Token::NumType::UNSIGNED_INTEGER:
        lit = static_cast<uint32_t>(std::stoi(number_str, 0, base));
        break;
      case Token::NumType::UNSIGNED_LONG:
        lit = static_cast<uint64_t>(std::stoull(number_str, 0, base));
        break;
      case Token::NumType::SIGNED_LONG:
        lit = static_cast<int64_t>(std::stoll(number_str, 0, base));
        break;
      case Token::NumType::SIGNED_INTEGER:
      default:
        lit = static_cast<int32_t>(std::stoi(number_str, 0, base));
    }
    make_token(Token::Type::NUMBER_LITERAL, lit);
    m_tokens.back().m_num_type = n_type;
  }
  else {
    make_token(Token::Type::FLOAT_LITERAL, std::stod(number_str));
    m_tokens.back().m_num_type = Token::NumType::DOUBLE;
  }
}

char Lexer::read_escape() {
  [[unlikely]]
  if (at_end())
    return '\0';

  char c = peek();
  switch (c) {
    case 'a':
      c = 0x07;
      advance();
      break;
    case 'b':
      c = 0x08;
      advance();
      break;
    case 'f':
      c = 0x0C;
      advance();
      break;
    case 'n':
      c = 0x0A;
      advance();
      break;
    case 'r':
      c = 0x0D;
      advance();
      break;
    case 't':
      c = 0x09;
      advance();
      break;
    case 'v':
      c = 0x0B;
      advance();
      break;
    case '\\':
      c = '\\';
      advance();
      break;
    case '\'':
      c = '\'';
      advance();
      break;
    case '"':
      c = '"';
      advance();
      break;
    case '?':
      c = '?';
      advance();
      break;
    case 'x':
      c = read_hex();
      break;
    default:
      if (is_octal_digit(c))
        c = read_octal();
      else
        // TODO Warn for unknown escape sequence
        // Simply ignore escape sequence if not valid
        break;
  }

  return c;
}

char Lexer::read_hex() {
  unsigned char c = 0;
  for (int i = 0; i < 2; ++i) {
    int val = hex_value(peek());
    if (val == -1) break;
    c = c << 4;
    advance();
    c = c | static_cast<unsigned char>(val);
  }

  return c;
}

char Lexer::read_octal() {
  unsigned int val = 0;
  for (int i = 0; i < 3; ++i) {
    char c = peek();
    if (!is_octal_digit(c)) break;
    val = val << 3;
    c = advance() - '0';
    val = val | c;
  }

  if (val > 255) {
    error("Octal escape sequence out of range");
  }

  return val;
}

void Lexer::preprocess() {
  Token current_token = m_tokens[0];

  std::vector<Token> new_tokens;

  bool if_macro_failed = false;
  int num_ifs = 0;

  if (m_macros == nullptr) {
    m_macros = std::make_shared<std::unordered_map<std::string, std::pair<int, MacroFuncPtr>>>();
  }

  // TODO Stringizing, Concatenation and Varadic Arguments not implemented

  while (current_token != Token::Type::EOFF) {
    current_token = next_token();
    bool consume_to_newline = false;

    if (current_token == Token::Type::IF_DEFINED || current_token == Token::Type::IF_NOT_DEFINED) {
      if (if_macro_failed) {
        num_ifs++;
        continue;
      }

      consume_to_newline = true;
      bool if_type = current_token == Token::Type::IF_DEFINED;

      current_token = next_token();
      if (current_token != Token::Type::IDENTIFIER) {
        error("Macro name must be an identifier");
      }
      else {
        auto it = m_macros->find(current_token.get_val<std::string>());

        // ifdef:    true == (macro not defined)
        // ifndef:   false == (macro defined)
        if (if_type == (it == m_macros->end())) {
          if_macro_failed = true;
        }
      }
    }
    else if (if_macro_failed) {
      // Do not include tokens for a failed if macro
      if (current_token != Token::Type::END_IF) continue;
      if (num_ifs == 0) {
        if_macro_failed = false;
      }
      else {
        num_ifs--;
      }
      consume_to_newline = true;
    }
    else if (current_token == Token::Type::DEFINE) {
      consume_to_newline = true;
      define();
    }
    else if (current_token == Token::Type::UNDEFINE) {
      consume_to_newline = true;
      undefine();
    }
    else if (current_token == Token::Type::INCLUDE) {
      consume_to_newline = true;
      std::vector<Token> included_file = include();

      new_tokens.insert(new_tokens.end(), included_file.begin(), included_file.end());
    }
    else if (current_token == Token::Type::IDENTIFIER) {
      std::vector<Token> expanded_macro = expand_macro(current_token);
      new_tokens.insert(new_tokens.end(), expanded_macro.begin(), expanded_macro.end());
    }
    else if (current_token == Token::Type::ERROR) {
      current_token = next_token();

      uint32_t start_column = current_token.m_source_index;
      while (current_token != Token::Type::NEWLINE && current_token != Token::Type::EOFF) {
        current_token = next_token();
      }
      uint32_t end_column = current_token.m_source_index;

      std::string message = m_source.substr(start_column, end_column - start_column);
      error(message);
    }
    else if (current_token != Token::Type::NEWLINE) {
      // Discard all newlines since they are purely for Preprocessing
      new_tokens.push_back(current_token);
    }

    if (consume_to_newline) {
      while (current_token != Token::Type::NEWLINE && current_token != Token::Type::EOFF) {
        current_token = next_token();
      }
      consume_to_newline = false;
    }
  }

  m_tokens = new_tokens;
  m_token_index = 0;
}

void Lexer::undefine() {
  Token current_token = next_token();
  if (current_token != Token::Type::IDENTIFIER) {
    error("Macro name must be an identifier");
  }
  else {
    m_macros->erase(current_token.get_val<std::string>());
  }
}

void Lexer::define() {
  Token current_token = next_token();
  if (current_token != Token::Type::IDENTIFIER) {
    error("Macro name must be an identifier");
    return;
  }

  std::string identifier = current_token.get_val<std::string>();
  std::vector<Token> args;
  std::vector<Token> body;

  current_token = next_token();
  // Function Like Macro
  if (current_token == Token::Type::OPEN_PAREN) {
    current_token = next_token();
    while (current_token != Token::Type::CLOSE_PAREN && current_token != Token::Type::EOFF) {
      if (current_token != Token::Type::IDENTIFIER) {
        error("Invalid macro parameter list Token Type");
        break;
      }
      args.push_back(current_token);
      current_token = next_token();
      if (current_token != Token::Type::COMMA && current_token != Token::Type::CLOSE_PAREN) {
        error("Expected Comma");
        break;
      }
      if (current_token == Token::Type::COMMA) {
        current_token = next_token();
      }
    }
    current_token = next_token();
  }

  while (current_token != Token::Type::NEWLINE && current_token != Token::Type::EOFF) {
    if (current_token == Token::Type::IDENTIFIER) {
      std::vector<Token> expanded_macro = expand_macro(current_token);
      body.insert(body.end(), expanded_macro.begin(), expanded_macro.end());
    }
    else {
      body.push_back(current_token);
    }
    current_token = next_token();
  }

  MacroFuncPtr macro = [args, body](std::vector<std::vector<Token>> params, size_t p_line, std::string p_filename) {
    std::vector<Token> expanded;
    for (size_t i = 0; i < body.size(); ++i) {
      if (body[i] == Token::Type::IDENTIFIER) {
        int pos = -1;
        for (size_t j = 0; j < args.size(); ++j) {
          if (args[j] == body[i]) {
            pos = j;
            break;
          }
        }
        if (pos != -1) {
          expanded.insert(expanded.end(), params[pos].begin(), params[pos].end());
        }
        else {
          expanded.push_back(body[i]);
        }
      }
      else {
        expanded.push_back(body[i]);
      }
    }

    for (auto it = expanded.begin(); it != expanded.end(); it++) {
      it->m_line = p_line;
      it->m_filename = p_filename;
    }
    return expanded;
  };

  m_macros->insert(std::make_pair(identifier, std::make_pair(args.size(), macro)));
}

// Assume all identifiers are macros at this point
std::vector<Token> Lexer::expand_macro(Token &p_identifier) {
  auto it = m_macros->find(p_identifier.get_val<std::string>());
  if (it == m_macros->end()) {
    return {p_identifier};
  }

  if (it->second.first == 0) {
    return it->second.second({}, p_identifier.m_line, p_identifier.m_filename);
  }

  Token current_token = next_token();
  if (current_token != Token::Type::OPEN_PAREN) {
    error("Expected '('");
    return {};
  }

  std::vector<std::vector<Token>> params;
  current_token = next_token();
  while (current_token != Token::Type::CLOSE_PAREN && current_token != Token::Type::EOFF) {
    std::vector<Token> current_param;
    while (current_token != Token::Type::COMMA && current_token != Token::Type::CLOSE_PAREN &&
           current_token != Token::Type::EOFF) {
      if (current_token == Token::Type::IDENTIFIER) {
        std::vector<Token> expanded_macro = expand_macro(current_token);
        current_param.insert(current_param.end(), expanded_macro.begin(), expanded_macro.end());
      }
      else {
        current_param.push_back(current_token);
      }

      current_token = next_token();
    }
    params.push_back(current_param);

    if (current_token == Token::Type::COMMA) current_token = next_token();
  }
  next_token();

  if (params.size() != static_cast<size_t>(it->second.first)) {
    error("Invalid number of macro Parameters.");
    return {};
  }

  return it->second.second(params, p_identifier.m_line, p_identifier.m_filename);
}

std::vector<Token> Lexer::include() {
  Token current_token = next_token();
  if (current_token == Token::Type::LESS) {
    // TODO standard library includes
    error("Standard Library Includes Not Yet Supported.");
    return {};
  }
  if (current_token != Token::Type::STRING_LITERAL) {
    error(R"(Expected "FILENAME" or <FILENAME>)");
    return {};
  }

  std::string include_filename =
      std::filesystem::path(m_filename).replace_filename(current_token.get_val<std::string>());
  Lexer lex(include_filename, m_macros);

  if (lex.scan()) {
    return {};
  }

  return std::move(lex.m_tokens);
}

static std::unordered_map<Token::Type, const char *> token_names = {
    {Token::Type::OPEN_PAREN, "OPEN_PAREN"},
    {Token::Type::CLOSE_PAREN, "CLOSE_PAREN"},
    {Token::Type::OPEN_CURLY_BRACE, "OPEN_CURLY_BRACE"},
    {Token::Type::CLOSE_CURLY_BRACE, "CLOSE_CURLY_BRACE"},
    {Token::Type::OPEN_SQUARE_BRACKET, "OPEN_SQUARE_BRACKET"},
    {Token::Type::CLOSE_SQUARE_BRACKET, "CLOSE_SQUARE_BRACKET"},
    {Token::Type::COMMA, "COMMA"},
    {Token::Type::DOT, "DOT"},
    {Token::Type::SEMICOLON, "SEMICOLON"},
    {Token::Type::TILDE, "TILDE"},
    {Token::Type::COLON, "COLON"},
    {Token::Type::QUESTION, "QUESTION"},
    {Token::Type::BANG, "BANG"},
    {Token::Type::BANG_EQUAL, "BANG_EQUAL"},
    {Token::Type::EQUAL, "EQUAL"},
    {Token::Type::EQUAL_EQUAL, "EQUAL_EQUAL"},
    {Token::Type::GREATER, "GREATER"},
    {Token::Type::GREATER_EQUAL, "GREATER_EQUAL"},
    {Token::Type::SHIFT_RIGHT, "SHIFT_RIGHT"},
    {Token::Type::SHIFT_RIGHT_EQUAL, "SHIFT_RIGHT_EQUAL"},
    {Token::Type::LESS, "LESS"},
    {Token::Type::LESS_EQUAL, "LESS_EQUAL"},
    {Token::Type::SHIFT_LEFT, "SHIFT_LEFT"},
    {Token::Type::SHIFT_LEFT_EQUAL, "SHIFT_LEFT_EQUAL"},
    {Token::Type::PLUS, "PLUS"},
    {Token::Type::PLUS_EQUAL, "PLUS_EQUAL"},
    {Token::Type::PLUS_PLUS, "PLUS_PLUS"},
    {Token::Type::MINUS, "MINUS"},
    {Token::Type::MINUS_EQUAL, "MINUS_EQUAL"},
    {Token::Type::MINUS_MINUS, "MINUS_MINUS"},
    {Token::Type::ARROW, "ARROW"},
    {Token::Type::STAR, "STAR"},
    {Token::Type::STAR_EQUAL, "STAR_EQUAL"},
    {Token::Type::SLASH, "SLASH"},
    {Token::Type::SLASH_EQUAL, "SLASH_EQUAL"},
    {Token::Type::PERCENT, "PERCENT"},
    {Token::Type::PERCENT_EQUAL, "PERCENT_EQUAL"},
    {Token::Type::AMPERSAND, "AMPERSAND"},
    {Token::Type::AMPERSAND_AMPERSAND, "AMPERSAND_AMPERSAND"},
    {Token::Type::AMPERSAND_EQUAL, "AMPERSAND_EQUAL"},
    {Token::Type::PIPE, "PIPE"},
    {Token::Type::PIPE_PIPE, "PIPE_PIPE"},
    {Token::Type::PIPE_EQUAL, "PIPE_EQUAL"},
    {Token::Type::CARET, "CARET"},
    {Token::Type::CARET_EQUAL, "CARET_EQUAL"},
    {Token::Type::IDENTIFIER, "IDENTIFIER"},
    {Token::Type::STRING_LITERAL, "STRING_LITERAL"},
    {Token::Type::NUMBER_LITERAL, "NUMBER_LITERAL"},
    {Token::Type::FLOAT_LITERAL, "FLOAT_LITERAL"},
    {Token::Type::AUTO, "AUTO"},
    {Token::Type::BREAK, "BREAK"},
    {Token::Type::CASE, "CASE"},
    {Token::Type::CHAR, "CHAR"},
    {Token::Type::CONST, "CONST"},
    {Token::Type::CONTINUE, "CONTINUE"},
    {Token::Type::DEFAULT, "DEFAULT"},
    {Token::Type::DO, "DO"},
    {Token::Type::DOUBLE, "DOUBLE"},
    {Token::Type::ELSE, "ELSE"},
    {Token::Type::ENUM, "ENUM"},
    {Token::Type::EXTERN, "EXTERN"},
    {Token::Type::FLOAT, "FLOAT"},
    {Token::Type::FOR, "FOR"},
    {Token::Type::GOTO, "GOTO"},
    {Token::Type::IF, "IF"},
    {Token::Type::INLINE, "INLINE"},
    {Token::Type::INT, "INT"},
    {Token::Type::LONG, "LONG"},
    {Token::Type::RETURN, "RETURN"},
    {Token::Type::SHORT, "SHORT"},
    {Token::Type::SIGNED, "SIGNED"},
    {Token::Type::SIZEOF, "SIZEOF"},
    {Token::Type::STATIC, "STATIC"},
    {Token::Type::STRUCT, "STRUCT"},
    {Token::Type::SWITCH, "SWITCH"},
    {Token::Type::TYPEDEF, "TYPEDEF"},
    {Token::Type::UNION, "UNION"},
    {Token::Type::UNSIGNED, "UNSIGNED"},
    {Token::Type::VOID, "VOID"},
    {Token::Type::VOLATILE, "VOLATILE"},
    {Token::Type::WHILE, "WHILE"},
    {Token::Type::NEWLINE, "NEWLINE"},
    {Token::Type::DEFINE, "DEFINE"},
    {Token::Type::UNDEFINE, "UNDEFINE"},
    {Token::Type::IF_DEFINED, "IF_DEFINED"},
    {Token::Type::IF_NOT_DEFINED, "IF_NOT_DEFINED"},
    {Token::Type::END_IF, "END_IF"},
    {Token::Type::ELLIPSIS, "ELLIPSIS"},
    {Token::Type::INCLUDE, "INCLUDE"},
    {Token::Type::ERROR, "ERROR"}};

std::string Token::get_type_string() const { return token_names[m_type]; }

std::string Token::string_repr() const {
  std::stringstream ss;
  ss << token_names[m_type] << "\t\t\"" << m_source << "\"\n";
  ss << "\t" << m_filename << ":" << m_line << ":" << m_column_start << "\n";

  if (m_type == Type::STRING_LITERAL) {
    ss << "\t\"" << get_val<std::string>() << "\"\n";
  }
  else if (m_type == Type::NUMBER_LITERAL) {
    ss << "\t\"" << get_val<uint64_t>() << "\"\n";
  }
  else if (m_type == Type::FLOAT_LITERAL) {
    ss << "\t\"" << get_val<double>() << "\"\n";
  }

  return ss.str();
}

}  // namespace JCC
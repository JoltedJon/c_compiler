#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace JCC {

using LiteralType =
    std::variant<int8_t, uint8_t, int16_t, uint16_t, int32_t, uint32_t, int64_t, uint64_t, float, double, std::string>;

struct Token {
  enum Type {
    // Punctuation
    OPEN_PAREN,            // (
    CLOSE_PAREN,           // )
    OPEN_CURLY_BRACE,      // {
    CLOSE_CURLY_BRACE,     // }
    OPEN_SQUARE_BRACKET,   // [
    CLOSE_SQUARE_BRACKET,  // ]
    DOT,                   // .
    ARROW,                 // ->
    SEMICOLON,             // ;

    // Modifiers
    TILDE,        // ~
    BANG,         // !
    PLUS,         // +
    PLUS_PLUS,    // ++
    MINUS,        // -
    MINUS_MINUS,  // --
    STAR,         // *
    SLASH,        // /
    PERCENT,      // %

    // Equality
    BANG_EQUAL,   // !=
    EQUAL_EQUAL,  // ==

    // Comparision
    GREATER,        // >
    GREATER_EQUAL,  // >=
    LESS,           // <
    LESS_EQUAL,     // <=

    // Bitwise
    SHIFT_RIGHT,  // >>
    SHIFT_LEFT,   // <<

    // Bitwise And
    AMPERSAND,  // &
    // Bitwise Xor
    CARET,  // ^
    // Bitwise Or
    PIPE,  // |
    // Logical And
    AMPERSAND_AMPERSAND,  // &&
    // Logical Or
    PIPE_PIPE,  // ||

    // Ternary
    COLON,     // :
    QUESTION,  // ?

    // Assignment
    EQUAL,              // =
    PLUS_EQUAL,         // +=
    MINUS_EQUAL,        // -=
    STAR_EQUAL,         // *=
    SLASH_EQUAL,        // /=
    PERCENT_EQUAL,      // %=
    AMPERSAND_EQUAL,    // &=
    PIPE_EQUAL,         // |=
    CARET_EQUAL,        // ^=
    SHIFT_RIGHT_EQUAL,  // >>=
    SHIFT_LEFT_EQUAL,   // <<=

    COMMA,  // ,

    // Literals.
    IDENTIFIER,      // variable names, function names, etc.
    STRING_LITERAL,  // "example"
    NUMBER_LITERAL,  // 123, 'c'
    FLOAT_LITERAL,   // 45.67

    // Keywords.
    AUTO,
    BREAK,
    CASE,
    CHAR,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    ELSE,
    ENUM,
    EXTERN,
    FLOAT,
    FOR,
    GOTO,
    IF,
    INLINE,
    INT,
    LONG,
    RETURN,
    SHORT,
    SIGNED,
    SIZEOF,
    STATIC,
    STRUCT,
    SWITCH,
    TYPEDEF,
    UNION,
    UNSIGNED,
    VOID,
    VOLATILE,
    WHILE,

    // Macros
    NEWLINE,
    DEFINE,
    UNDEFINE,
    IF_DEFINED,
    IF_NOT_DEFINED,
    END_IF,
    ELLIPSIS,
    INCLUDE,
    ERROR,

    // Special
    TYPEDEF_NAME,

    EOFF
  };
  enum class NumType {
    NONE,
    SIGNED_INTEGER,
    UNSIGNED_INTEGER,
    SIGNED_LONG,
    UNSIGNED_LONG,
    DOUBLE,
  };

  std::string m_filename;
  uint32_t m_line;
  uint32_t m_column_start;
  uint32_t m_source_index;

  Type m_type;
  NumType m_num_type;
  std::string m_source;
  LiteralType m_literal;

  Token() : m_type(Type::EOFF) {}
  Token(std::string p_filename, uint32_t p_line, uint32_t p_column_start, uint32_t p_source_index, Type p_type,
        std::string p_source, LiteralType p_literal)
      : m_filename(p_filename),
        m_line(p_line),
        m_column_start(p_column_start),
        m_source_index(p_source_index),
        m_type(p_type),
        m_num_type(NumType::NONE),
        m_source(p_source),
        m_literal(p_literal) {}

  std::string string_repr() const;
  std::string get_type_string() const;

  template <typename T>
  T get_val() const {
    return std::holds_alternative<T>(m_literal) ? std::get<T>(m_literal) : T();
  }

  inline bool operator==(Type const &p_type) const { return m_type == p_type; }
  inline bool operator!=(Type const &p_type) const { return m_type != p_type; }

  inline bool operator==(Token const &p_rhs) const {
    if (m_type == p_rhs.m_type) {
      if (m_type == Type::IDENTIFIER) {
        return get_val<std::string>() == p_rhs.get_val<std::string>();
      }
      else if (m_type == Type::FLOAT_LITERAL) {
        return get_val<double>() == p_rhs.get_val<double>();
      }
      else if (m_type == Type::NUMBER_LITERAL) {
        return get_val<uint64_t>() == p_rhs.get_val<uint64_t>();
      }
      return true;
    }
    return false;
  }
  inline bool operator!=(Token const &p_rhs) const { return !(operator==(p_rhs)); }

  inline bool operator==(const std::string &p_rhs) const {
    if (m_type != IDENTIFIER && m_type != STRING_LITERAL) return false;
    return get_val<std::string>() == p_rhs;
  }
  inline bool operator!=(const std::string &p_rhs) const { return !operator==(p_rhs); }
};

using MacroFuncPtr = std::function<std::vector<Token>(std::vector<std::vector<Token>>, size_t, std::string)>;

class Lexer {
 public:
  Lexer(const std::string &p_filename);
  Lexer(const std::string &p_filename,
        std::shared_ptr<std::unordered_map<std::string, std::pair<int, MacroFuncPtr>>> p_macros);
  bool scan();

  inline Token next_token() { return m_token_index < m_tokens.size() ? m_tokens[m_token_index++] : Token(); }

  std::string m_filename;

  std::string m_source;
  size_t m_start;
  size_t m_current;

  size_t m_line;
  size_t m_column_start;
  size_t m_column_end;

  std::vector<Token> m_tokens;
  size_t m_token_index;

  bool m_error;

  std::shared_ptr<std::unordered_map<std::string, std::pair<int, MacroFuncPtr>>> m_macros;

 private:
  void error(const std::string p_message);

  inline bool at_end() { return m_current >= m_source.size(); }
  inline char peek() { return at_end() ? '\0' : m_source[m_current]; }
  inline char peek(int p_offset) {
    return at_end() || static_cast<int64_t>(m_current + p_offset) < 0 || m_current + p_offset >= m_source.size()
               ? '\0'
               : m_source[m_current + p_offset];
  }
  char advance();
  bool match(char p_c);

  void scan_token();

  void make_token(Token::Type p_type);
  void make_token(Token::Type p_type, LiteralType p_literal);

  Token::NumType read_integer_suffix();

  void read_number();
  void read_char();
  void read_string();
  void read_identifier();
  void read_macro();
  void read_multiline_comment();

  char read_escape();
  char read_hex();
  char read_octal();

  void preprocess();
  void undefine();
  void define();

  std::vector<Token> expand_macro(Token &p_identifier);
  std::vector<Token> include();
};

}  // namespace JCC
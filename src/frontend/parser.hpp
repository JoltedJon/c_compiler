#pragma once

#include <memory>
#include <optional>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include "../ast.hpp"
#include "lexer.hpp"

namespace JCC {

class Parser {
 private:
  void error(const std::string &p_error_message);
  void warning(const std::string &p_message);

  bool m_has_error = false;
  bool m_panic_mode = false;

  template <typename T>
    requires std::is_base_of_v<Node, T>
  T *alloc_node() {
    T *node = new T();

    node->m_line = m_previous_tok.m_line;
    node->m_column_start = m_previous_tok.m_column_start;
    node->m_filename = m_previous_tok.m_filename;

    return node;
  }

  using ParseFunction = UniqueExpression (Parser::*)(UniqueExpression p_previous_operand);

  enum Precedence {
    PREC_NONE,
    PREC_COMMA,
    PREC_ASSIGNMENT,
    PREC_TERNARY,
    PREC_LOGIC_OR,
    PREC_LOGIC_AND,
    PREC_BIT_OR,
    PREC_BIT_XOR,
    PREC_BIT_AND,
    PREC_EQUALITY,
    PREC_RELATIONAL,
    PREC_BIT_SHIFT,
    PREC_ADDITION,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
  };
  struct ParseRule {
    ParseFunction prefix = nullptr;
    ParseFunction infix = nullptr;
    Precedence precedence = Precedence::PREC_NONE;
  };
  ParseRule *get_rule(Token::Type p_token_type);

  // Expression Parsing
  UniqueExpression parse_expression();
  UniqueExpression parse_precedence(Precedence p_precedence);
  UniqueExpression parse_grouping(UniqueExpression p_previous_operand);
  UniqueExpression parse_call(UniqueExpression p_previous_operand);
  UniqueExpression parse_subscript(UniqueExpression p_previous_operand);
  UniqueExpression parse_binary_operation(UniqueExpression p_previous_operand);
  UniqueExpression parse_assignment_operation(UniqueExpression p_previous_operand);
  UniqueExpression parse_unary_operation(UniqueExpression p_previous_operand);
  UniqueExpression parse_ternary_operator(UniqueExpression p_previous_operand);
  UniqueExpression parse_member_access(UniqueExpression p_previous_operand);
  UniqueExpression parse_postfix(UniqueExpression p_previous_operand);
  UniqueExpression parse_identifier(UniqueExpression p_previous_operand);
  UniqueExpression parse_literal(UniqueExpression p_previous_operand);

  // Statement Parsing
  UniqueStatement parse_statement();
  UniqueStatement parse_if_statement();
  UniqueStatement parse_while_statement();
  UniqueStatement parse_for_statement();
  UniqueStatement parse_switch_statement();
  UniqueStatement parse_return_statement();
  UniqueStatement parse_label_statement(const std::string &p_label);
  UniqueStatement parse_goto_statement();
  UniqueStatement parse_case_statement();
  UniqueStatement parse_default_statement();
  UniqueStatement parse_control_statement();
  UniqueStatement parse_expression_statement();
  UniqueStatement parse_compound_statement();

  // Declaration Parsing
  bool is_declaration_start();
  UniqueDeclaration parse_declaration();

  DeclarationNode::StorageClassSpec get_storage_spec();
  TypeQualifier get_type_qualifier();
  Token get_type_specifier();
  SharedDataType parse_type_specifiers(std::vector<Token> &p_type_specifiers);
  SharedDataType parse_struct_union(DataType::TypeKind p_kind);
  SharedDataType parse_enum();

  SharedDataType parse_declaration_specifiers(DeclarationNode::StorageClassSpec &p_spec);
  UniqueDeclaration parse_declarator(SharedDataType p_base_type);
  SharedDataType parse_pointer(SharedDataType p_base_type);
  UniqueDeclaration parse_function(SharedDataType p_return_type, const std::string &p_identifier);
  UniqueDeclaration parse_array(SharedDataType p_base_type, const std::string &p_identifier);

  // Program parsing
  UniqueNode parse_decl_stmt();

  // Parsing Context
  // A new context is pushed when entering into a function declaration
  struct TypeContext {
    std::unordered_map<std::string, SharedDataType> m_type_symbol_table;
    std::unordered_map<std::string, int> m_enumeration_constants;

    std::unique_ptr<TypeContext> m_previous_context = nullptr;
  };

  std::unique_ptr<TypeContext> m_current_type_context;
  inline void push_type_context() {
    std::unique_ptr<TypeContext> next_context = std::make_unique<TypeContext>();
    next_context->m_previous_context = std::move(m_current_type_context);
    m_current_type_context = std::move(next_context);
  }
  inline void pop_type_context() { m_current_type_context = std::move(m_current_type_context->m_previous_context); }

  inline void push_type(const std::string &p_name, SharedDataType p_new_type) {
    m_current_type_context->m_type_symbol_table[p_name] = p_new_type;
  }

  // Gets a type from the current context to allow for shadowing of types in previous context
  inline SharedDataType get_type_cur_context(const std::string &p_name) {
    auto it = m_current_type_context->m_type_symbol_table.find(p_name);
    if (it == m_current_type_context->m_type_symbol_table.end()) {
      return nullptr;
    }
    return it->second;
  }

  inline SharedDataType get_type(const std::string &p_name) {
    TypeContext *context = m_current_type_context.get();
    while (context != nullptr) {
      auto it = context->m_type_symbol_table.find(p_name);
      if (it != context->m_type_symbol_table.end()) {
        return it->second;
      }
      context = context->m_previous_context.get();
    }

    return nullptr;
  }

  std::optional<int> get_enum_val(const std::string &p_name) {
    TypeContext *context = m_current_type_context.get();
    while (context != nullptr) {
      auto it = context->m_enumeration_constants.find(p_name);
      if (it != context->m_enumeration_constants.end()) {
        return it->second;
      }
      context = context->m_previous_context.get();
    }

    return {};
  }

  inline void set_enum_val(const std::string &p_name, int value) {
    m_current_type_context->m_enumeration_constants[p_name] = value;
  }

  inline void register_base_type(const std::string &p_name, DataType::TypeKind p_kind, bool p_is_signed,
                                 unsigned int p_size) {
    push_type(p_name, std::make_shared<DataType>(p_kind, p_is_signed, p_size));
  }

  // Tokens
 private:
  std::vector<Token> m_tokens;
  Token m_previous_tok;
  Token m_current_tok;
  size_t m_token_index = 0;

  Token advance();
  bool match(Token::Type p_token_type);
  bool check(Token::Type p_token_type) const;
  bool consume(Token::Type p_token_type, const std::string &p_error_message);
  void synchronize();

 public:
  inline bool is_at_end() const { return m_token_index >= m_tokens.size(); }
  Parser(Lexer &l);

  UniqueNode parse();
  inline bool had_error() const { return m_has_error; }
  std::unordered_map<std::string, SharedDataType> get_base_types() const {
    return m_current_type_context->m_type_symbol_table;
  }

  void graph_gen(std::ostream &out, const Node *node) const;
};

}  // namespace JCC
#include "parser.hpp"

#include <bit>
#include <cassert>
#include <cstdlib>
#include <format>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "ansi_colors.hpp"
#include "lexer.hpp"

namespace JCC {

#define fatal_error(p_error_message)                                                                 \
  do {                                                                                               \
    std::cerr << ANSI_COLOR_RED << "Fatal Error - Please Report Bug: " << ANSI_COLOR_RESET << "\n\t" \
              << __PRETTY_FUNCTION__ << ":" << __LINE__ << "\n\t" << std::endl;                      \
    error(p_error_message);                                                                          \
    std::abort();                                                                                    \
  } while (0)

void Parser::error(const std::string &p_error_message) {
  m_has_error = true;
  m_panic_mode = true;

  std::cerr << ANSI_COLOR_RED << "Error: " << ANSI_COLOR_RESET
            << std::format("{} {}:{} {}\n{}", m_current_tok.m_filename, m_current_tok.m_line,
                           m_current_tok.m_column_start, m_current_tok.m_source, p_error_message)
            << std::endl;
}

void Parser::warning(const std::string &p_message) {
  std::cerr << ANSI_COLOR_YELLOW << "Error: " << ANSI_COLOR_RESET
            << std::format("{} {}:{} {}\n{}", m_current_tok.m_filename, m_current_tok.m_line,
                           m_current_tok.m_column_start, m_current_tok.m_source, p_message)
            << std::endl;
}

Parser::Parser(Lexer &lex) {
  // Steal lexer's tokens since the lexer no longer needs them
  m_tokens = std::move(lex.m_tokens);
  m_token_index = 0;
  m_current_tok = m_tokens[m_token_index++];
  m_previous_tok = m_current_tok;

  m_current_type_context = std::make_unique<TypeContext>();

  // Register builtin datatypes
  register_base_type("unsigned char", DataType::TypeKind::TYPE_INT, false, 1);
  register_base_type("signed char", DataType::TypeKind::TYPE_INT, true, 1);

  register_base_type("unsigned short", DataType::TypeKind::TYPE_INT, false, 2);
  register_base_type("signed short", DataType::TypeKind::TYPE_INT, true, 2);

  register_base_type("unsigned int", DataType::TypeKind::TYPE_INT, false, 4);
  register_base_type("signed int", DataType::TypeKind::TYPE_INT, true, 4);

  register_base_type("unsigned long", DataType::TypeKind::TYPE_INT, false, 8);
  register_base_type("signed long", DataType::TypeKind::TYPE_INT, true, 8);

  register_base_type("unsigned float", DataType::TypeKind::TYPE_FLOAT, false, 4);
  register_base_type("signed float", DataType::TypeKind::TYPE_FLOAT, true, 4);

  register_base_type("unsigned double", DataType::TypeKind::TYPE_FLOAT, false, 8);
  register_base_type("signed double", DataType::TypeKind::TYPE_FLOAT, true, 8);

  register_base_type("void", DataType::TypeKind::TYPE_VOID, false, 0);

  SharedDataType const_char = get_type("unsigned char")->clone();
  const_char->set_qualifiers({TypeQualifier::CONST});
  PointerType *string_type = new PointerType();
  string_type->m_base_type = const_char;
  push_type("string type", SharedDataType(string_type));

  register_base_type("unresolved type", DataType::TypeKind::TYPE_UNRESOLVED, false, 0);
}

Parser::UniqueNode Parser::parse() {
  std::vector<UniqueDeclaration> program;

  while (!is_at_end()) {
    program.push_back(parse_declaration());
    if (m_panic_mode) {
      synchronize();
    }
  }

  TranslationUnit *unit = alloc_node<TranslationUnit>();
  unit->program = std::move(program);

  return UniqueNode(unit);
}

Parser::ParseRule *Parser::get_rule(Token::Type p_token_type) {
  static ParseRule rules[] = {
      // prefix   infix   precedence
      // Punctuation
      {&Parser::parse_grouping, &Parser::parse_call, PREC_CALL},  //     LEFT_PAREN
      {nullptr, nullptr, PREC_NONE},                              //     RIGHT_PAREN
      {nullptr, nullptr, PREC_NONE},                              //     LEFT_BRACE {
      {nullptr, nullptr, PREC_NONE},                              //     RIGHT_BRACE }
      {nullptr, &Parser::parse_subscript, PREC_CALL},             //     LEFT_BRACKET [
      {nullptr, nullptr, PREC_NONE},                              //     RIGHT_BRACKET ]
      {nullptr, &Parser::parse_member_access, PREC_CALL},         //     DOT
      {nullptr, &Parser::parse_member_access, PREC_CALL},         //     ARROW
      {nullptr, nullptr, PREC_NONE},                              //     SEMICOLON

      {&Parser::parse_unary_operation, nullptr, PREC_UNARY},                             //     TILDE
      {&Parser::parse_unary_operation, nullptr, PREC_UNARY},                             //     BANG
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, PREC_ADDITION},  //     PLUS
      {&Parser::parse_unary_operation, &Parser::parse_postfix, PREC_CALL},               //     PLUS_PLUS
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, PREC_ADDITION},  //     MINUS
      {&Parser::parse_unary_operation, &Parser::parse_postfix, PREC_CALL},               //     MINUS_MINUS
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, PREC_FACTOR},    //     STAR
      {nullptr, &Parser::parse_binary_operation, PREC_FACTOR},                           //     SLASH
      {nullptr, &Parser::parse_binary_operation, PREC_FACTOR},                           //     PERCENT

      // Equality
      {nullptr, &Parser::parse_binary_operation, PREC_EQUALITY},  //     BANG_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_EQUALITY},  //     EQUAL_EQUAL

      // Comparision
      {nullptr, &Parser::parse_binary_operation, PREC_RELATIONAL},  //     GREATER
      {nullptr, &Parser::parse_binary_operation, PREC_RELATIONAL},  //     GREATER_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_RELATIONAL},  //     LESS
      {nullptr, &Parser::parse_binary_operation, PREC_RELATIONAL},  //     LESS_EQUAL

      // Bitwise
      {nullptr, &Parser::parse_binary_operation, PREC_BIT_SHIFT},  //     SHIFT_RIGHT
      {nullptr, &Parser::parse_binary_operation, PREC_BIT_SHIFT},  //     SHIFT_LEFT

      // Logic and Bitwise
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, PREC_BIT_AND},  //     AMPERSAND
      {nullptr, &Parser::parse_binary_operation, PREC_BIT_XOR},                         //     CARET
      {nullptr, &Parser::parse_binary_operation, PREC_BIT_OR},                          //     PIPE
      {nullptr, &Parser::parse_binary_operation, PREC_LOGIC_AND},                       //     AMPERSAND_AMPERSAND
      {nullptr, &Parser::parse_binary_operation, PREC_LOGIC_OR},                        //     PIPE_PIPE

      // Ternary
      {nullptr, nullptr, PREC_TERNARY},                          //     COLON
      {nullptr, &Parser::parse_ternary_operator, PREC_TERNARY},  //     QUESTION

      // Assignment
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     PLUS_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     MINUS_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     STAR_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     SLASH_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     PERCENT_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     AMPERSAND_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     PIPE_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     CARET_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     SHIFT_RIGHT_EQUAL
      {nullptr, &Parser::parse_binary_operation, PREC_ASSIGNMENT},  //     SHIFT_LEFT_EQUAL

      // Comma
      {nullptr, nullptr, PREC_COMMA},  //     COMMA

      // Literals
      {&Parser::parse_identifier, nullptr, PREC_NONE},  //     IDENTIFIER
      {&Parser::parse_literal, nullptr, PREC_NONE},     //     STRING_LITERAL
      {&Parser::parse_literal, nullptr, PREC_NONE},     //     NUMBER_LITERAL
      {&Parser::parse_literal, nullptr, PREC_NONE},     //     FLOAT_LITERAL

      // Keywords
      {nullptr, nullptr, PREC_NONE},                         //     AUTO
      {nullptr, nullptr, PREC_NONE},                         //     BREAK
      {nullptr, nullptr, PREC_NONE},                         //     CASE
      {nullptr, nullptr, PREC_NONE},                         //     CHAR
      {nullptr, nullptr, PREC_NONE},                         //     CONST
      {nullptr, nullptr, PREC_NONE},                         //     CONTINUE
      {nullptr, nullptr, PREC_NONE},                         //     DEFAULT
      {nullptr, nullptr, PREC_NONE},                         //     DO
      {nullptr, nullptr, PREC_NONE},                         //     DOUBLE
      {nullptr, nullptr, PREC_NONE},                         //     ELSE
      {nullptr, nullptr, PREC_NONE},                         //     ENUM
      {nullptr, nullptr, PREC_NONE},                         //     EXTERN
      {nullptr, nullptr, PREC_NONE},                         //     FLOAT
      {nullptr, nullptr, PREC_NONE},                         //     FOR
      {nullptr, nullptr, PREC_NONE},                         //     GOTO
      {nullptr, nullptr, PREC_NONE},                         //     IF
      {nullptr, nullptr, PREC_NONE},                         //     INLINE
      {nullptr, nullptr, PREC_NONE},                         //     INT
      {nullptr, nullptr, PREC_NONE},                         //     LONG
      {nullptr, nullptr, PREC_NONE},                         //     RETURN
      {nullptr, nullptr, PREC_NONE},                         //     SHORT
      {nullptr, nullptr, PREC_NONE},                         //     SIGNED
      {&Parser::parse_unary_operation, nullptr, PREC_NONE},  //     SIZEOF
      {nullptr, nullptr, PREC_NONE},                         //     STATIC
      {nullptr, nullptr, PREC_NONE},                         //     STRUCT
      {nullptr, nullptr, PREC_NONE},                         //     SWITCH
      {nullptr, nullptr, PREC_NONE},                         //     TYPEDEF
      {nullptr, nullptr, PREC_NONE},                         //     UNION
      {nullptr, nullptr, PREC_NONE},                         //     UNSIGNED
      {nullptr, nullptr, PREC_NONE},                         //     VOID
      {nullptr, nullptr, PREC_NONE},                         //     VOLATILE
      {nullptr, nullptr, PREC_NONE},                         //     WHILE

      // Macros
      {nullptr, nullptr, PREC_NONE},  //     NEWLINE
      {nullptr, nullptr, PREC_NONE},  //     DEFINE
      {nullptr, nullptr, PREC_NONE},  //     UNDEFINE
      {nullptr, nullptr, PREC_NONE},  //     IF_DEFINED
      {nullptr, nullptr, PREC_NONE},  //     IF_NOT_DEFINED
      {nullptr, nullptr, PREC_NONE},  //     END_IF
      {nullptr, nullptr, PREC_NONE},  //     ELLIPSIS,
      {nullptr, nullptr, PREC_NONE},  //     INCLUDE
      {nullptr, nullptr, PREC_NONE},  //     ERROR

      {nullptr, nullptr, PREC_NONE},  //     TYPEDEF_NAME
  };

  static_assert(sizeof(rules) / sizeof(rules[0]) == static_cast<int>(Token::Type::EOFF),
                "Amount of parse rules don't match the amount of token types.");

  return &rules[p_token_type];
}

Parser::UniqueExpression Parser::parse_expression() { return parse_precedence(PREC_COMMA); }

Parser::UniqueExpression Parser::parse_precedence(Parser::Precedence p_precedence) {
  Token token = m_current_tok;
  Token::Type type = token.m_type;

  ParseFunction prefix_rule = get_rule(type)->prefix;

  if (!prefix_rule) {
    // Error that caller should handle.
    return nullptr;
  }

  advance();

  UniqueExpression previous_operand = (this->*prefix_rule)(nullptr);

  while (p_precedence <= get_rule(m_current_tok.m_type)->precedence) {
    if (previous_operand == nullptr) {
      return previous_operand;
    }

    token = advance();
    ParseFunction infix_rule = get_rule(token.m_type)->infix;
    previous_operand = (this->*infix_rule)(std::move(previous_operand));
  }

  return previous_operand;
}

Parser::UniqueExpression Parser::parse_grouping(Parser::UniqueExpression p_previous_operand) {
  // TODO Cast Expressions
  // Requires knowing types

  UniqueExpression expr = parse_expression();

  consume(Token::Type::CLOSE_PAREN, R"-(Expected ")".)-");

  if (expr->d_type == nullptr) {
    expr->d_type = get_type("unresolved type");
  }

  return expr;
}

Parser::UniqueExpression Parser::parse_call(Parser::UniqueExpression p_previous_operand) {
  CallNode *call = alloc_node<CallNode>();

  call->callee = std::move(p_previous_operand);

  if (call->callee == nullptr) {
    error("Expected Expression on call.");
  }
  else if (call->callee->node_type == Node::NodeType::IDENTIFIER) {
    call->name = dynamic_cast<IdentifierNode &>(*call->callee).name;
  }

  do {
    if (check(Token::Type::CLOSE_PAREN)) {
      break;
    }
    UniqueExpression argument = parse_precedence(PREC_ASSIGNMENT);

    if (argument == nullptr) {
      error("Expected Expression as function argument.");
    }
    else {
      call->args.push_back(std::move(argument));
    }
  } while (match(Token::Type::COMMA));

  consume(Token::Type::CLOSE_PAREN, R"*(Expected closing ")" after function call arguments.)*");

  call->d_type = get_type("unresolved type");

  return UniqueExpression(call);
}

Parser::UniqueExpression Parser::parse_subscript(Parser::UniqueExpression p_previous_operand) {
  BinaryOpNode *operation = alloc_node<BinaryOpNode>();
  operation->operation = BinaryOpNode::OpType::OP_ARRAY_SUBSCRIPT;
  operation->left_operand = std::move(p_previous_operand);
  operation->right_operand = parse_expression();

  consume(Token::Type::CLOSE_SQUARE_BRACKET, R"(Expected "]" after array subscript expression.)");

  operation->d_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

Parser::UniqueExpression Parser::parse_binary_operation(Parser::UniqueExpression p_previous_operand) {
  static std::unordered_map<Token::Type, BinaryOpNode::OpType> binaryOps = {
      {Token::Type::PLUS, BinaryOpNode::OpType::OP_ADDITION},
      {Token::Type::MINUS, BinaryOpNode::OpType::OP_SUBTRACTION},
      {Token::Type::STAR, BinaryOpNode::OpType::OP_MULTIPLICATION},
      {Token::Type::SLASH, BinaryOpNode::OpType::OP_DIVISION},
      {Token::Type::PERCENT, BinaryOpNode::OpType::OP_MODULO},
      {Token::Type::SHIFT_RIGHT, BinaryOpNode::OpType::OP_BIT_RIGHT},
      {Token::Type::SHIFT_LEFT, BinaryOpNode::OpType::OP_BIT_LEFT},
      {Token::Type::GREATER, BinaryOpNode::OpType::OP_COMP_GREATER},
      {Token::Type::LESS, BinaryOpNode::OpType::OP_COMP_LESS},
      {Token::Type::GREATER_EQUAL, BinaryOpNode::OpType::OP_COMP_GREATER_EQUAL},
      {Token::Type::LESS_EQUAL, BinaryOpNode::OpType::OP_COMP_LESS_EQUAL},
      {Token::Type::EQUAL_EQUAL, BinaryOpNode::OpType::OP_COMP_EQUAL},
      {Token::Type::BANG_EQUAL, BinaryOpNode::OpType::OP_COMP_NOT_EQUAL},
      {Token::Type::AMPERSAND, BinaryOpNode::OpType::OP_BIT_AND},
      {Token::Type::CARET, BinaryOpNode::OpType::OP_BIT_XOR},
      {Token::Type::PIPE, BinaryOpNode::OpType::OP_BIT_OR},
      {Token::Type::AMPERSAND_AMPERSAND, BinaryOpNode::OpType::OP_LOGIC_AND},
      {Token::Type::PIPE_PIPE, BinaryOpNode::OpType::OP_LOGIC_OR},
      {Token::Type::EQUAL, BinaryOpNode::OpType::OP_ASSIGN},
      {Token::Type::PLUS_EQUAL, BinaryOpNode::OpType::OP_ADD_ASSIGN},
      {Token::Type::MINUS_EQUAL, BinaryOpNode::OpType::OP_SUBTRACT_ASSIGN},
      {Token::Type::STAR_EQUAL, BinaryOpNode::OpType::OP_MULTIPLY_ASSIGN},
      {Token::Type::SLASH_EQUAL, BinaryOpNode::OpType::OP_DIVIDE_ASSIGN},
      {Token::Type::PERCENT_EQUAL, BinaryOpNode::OpType::OP_MODULO_ASSIGN},
      {Token::Type::AMPERSAND_EQUAL, BinaryOpNode::OpType::OP_BITWISE_AND_ASSIGN},
      {Token::Type::PIPE_EQUAL, BinaryOpNode::OpType::OP_BITWISE_OR_ASSIGN},
      {Token::Type::CARET_EQUAL, BinaryOpNode::OpType::OP_BITWISE_XOR_ASSIGN},
      {Token::Type::SHIFT_RIGHT_EQUAL, BinaryOpNode::OpType::OP_LEFT_SHIFT_ASSIGN},
      {Token::Type::SHIFT_LEFT_EQUAL, BinaryOpNode::OpType::OP_RIGHT_SHIFT_ASSIGN}};

  Token op = m_previous_tok;
  BinaryOpNode *operation = alloc_node<BinaryOpNode>();

  Precedence precedence = (Precedence)(get_rule(op.m_type)->precedence);
  operation->left_operand = std::move(p_previous_operand);
  operation->right_operand = parse_precedence(precedence);

  if (operation->right_operand == nullptr) {
    error(std::format(R"(Expected expression after "{}" operator.)", op.get_type_string()));
  }

  auto it = binaryOps.find(op.m_type);

  [[unlikely]]
  if (it == binaryOps.end()) {
    fatal_error(std::format("Unexpected Token: {}", op.get_type_string()));
  }

  operation->operation = it->second;
  operation->d_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

Parser::UniqueExpression Parser::parse_unary_operation(Parser::UniqueExpression p_previous_operand) {
  Token::Type op_type = m_previous_tok.m_type;
  UnaryOpNode *operation = alloc_node<UnaryOpNode>();
  operation->operand = parse_precedence(PREC_UNARY);

  switch (op_type) {
    case Token::Type::PLUS:
      operation->operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->operand) {
        error(R"(Expected expression after "+" operator.)");
      }
      break;
    case Token::Type::MINUS:
      operation->operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->operand) {
        error(R"(Expected expression after "-" operator.)");
      }
      break;
    case Token::Type::PLUS_PLUS:
      operation->operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->operand) {
        error(R"(Expected expression after "++" operator.)");
      }
      break;
    case Token::Type::MINUS_MINUS:
      operation->operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->operand) {
        error(R"(Expected expression after "--" operator.)");
      }
      break;
    case Token::Type::BANG:
      operation->operation = UnaryOpNode::OpType::OP_LOGIC_NOT;
      if (!operation->operand) {
        error(R"(Expected expression after "!" operator.)");
      }
      break;
    case Token::Type::TILDE:
      operation->operation = UnaryOpNode::OpType::OP_LOGIC_NOT;
      if (!operation->operand) {
        error(R"(Expected expression after "~" operator.)");
      }
      break;
    case Token::Type::AMPERSAND:
      operation->operation = UnaryOpNode::OpType::OP_ADDRESS_OF;
      if (!operation->operand) {
        error(R"(Expected expression after "&" operator.)");
      }
      break;
    case Token::Type::STAR:
      operation->operation = UnaryOpNode::OpType::OP_INDIRECTION;
      if (!operation->operand) {
        error(R"(Expected expression after "*" operator.)");
      }
      break;
    case Token::Type::SIZEOF:
      operation->operation = UnaryOpNode::OpType::OP_SIZEOF;
      // TODO Parse this expression
      // sizeof ( type-name )
      if (!operation->operand) {
        error(R"(Expected expression after "*" operator.)");
      }
      break;
    default:
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  operation->d_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

Parser::UniqueExpression Parser::parse_ternary_operator(Parser::UniqueExpression p_previous_operand) {
  TernaryOpNode *operation = alloc_node<TernaryOpNode>();

  operation->condition = std::move(p_previous_operand);
  operation->true_expr = parse_precedence(PREC_TERNARY);

  if (operation->true_expr == nullptr) {
    error(R"(Expected expression after "?" operator.)");
  }

  consume(Token::Type::COLON, R"(Expected ":".)");

  operation->false_expr = parse_precedence(PREC_TERNARY);

  if (operation->false_expr == nullptr) {
    error(R"(Expected expression after ":".)");
  }

  operation->d_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

Parser::UniqueExpression Parser::parse_member_access(Parser::UniqueExpression p_previous_operand) {
  Token op = m_previous_tok;

  BinaryOpNode *operation = alloc_node<BinaryOpNode>();
  operation->left_operand = std::move(p_previous_operand);
  operation->right_operand = parse_expression();

  switch (op.m_type) {
    case Token::Type::DOT:
      operation->operation = BinaryOpNode::OpType::OP_DIRECT_MEM_ACCESS;
      break;
    case Token::Type::ARROW:
      operation->operation = BinaryOpNode::OpType::OP_INDIRECT_MEM_ACCESS;
      break;
    default:
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  if (operation->right_operand->node_type != Node::NodeType::IDENTIFIER) {
    error("Member access must be an identifier");
  }

  operation->d_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

Parser::UniqueExpression Parser::parse_postfix(Parser::UniqueExpression p_previous_operand) {
  Token op = m_previous_tok;

  UnaryOpNode *operation = alloc_node<UnaryOpNode>();
  operation->operand = std::move(p_previous_operand);

  switch (op.m_type) {
    case Token::Type::PLUS_PLUS:
      operation->operation = UnaryOpNode::OpType::OP_POST_INCREMENT;
      break;
    case Token::Type::MINUS_MINUS:
      operation->operation = UnaryOpNode::OpType::OP_POST_DECREMENT;
      break;
    default:
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  operation->d_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

Parser::UniqueExpression Parser::parse_identifier(Parser::UniqueExpression p_previous_operand) {
  std::optional<int> opt_value = get_enum_val(m_previous_tok.get_val<std::string>());
  if (opt_value.has_value()) {
    ConstantNode *constant = alloc_node<ConstantNode>();
    constant->value = *opt_value;
    constant->val_type = ConstantNode::ValType::INTEGER;
    constant->d_type = get_type("signed int");
    return UniqueExpression(constant);
  }

  IdentifierNode *ident = alloc_node<IdentifierNode>();
  ident->name = m_previous_tok.get_val<std::string>();

  if (ident->name == "") {
    error("Identifier name is Empty.");
  }

  ident->d_type = get_type("unresolved type");

  return UniqueExpression(ident);
}

Parser::UniqueExpression Parser::parse_literal(Parser::UniqueExpression p_previous_operand) {
  Token lit = m_previous_tok;
  ConstantNode *constant = alloc_node<ConstantNode>();
  constant->value = m_previous_tok.m_literal;

  switch (lit.m_type) {
    case Token::Type::NUMBER_LITERAL: {
      constant->val_type = ConstantNode::ValType::INTEGER;
      uint64_t num = m_previous_tok.get_val<uint64_t>();
      int bits = std::bit_width(num);
      if (bits <= 8) {
        constant->d_type = get_type("unsigned char");
      }
      else if (bits <= 16) {
        constant->d_type = get_type("unsigned short");
      }
      else if (bits <= 32) {
        constant->d_type = get_type("unsigned int");
      }
      else if (bits <= 64) {
        constant->d_type = get_type("unsigned long");
      }
    } break;
    case Token::Type::FLOAT_LITERAL:
      constant->val_type = ConstantNode::ValType::FLOAT;
      constant->d_type = get_type("unsigned double");
      break;
    case Token::Type::STRING_LITERAL:
      constant->val_type = ConstantNode::ValType::STRING;
      constant->d_type = get_type("string type");
      break;
    default:
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  return UniqueExpression(constant);
}

// Statements
Parser::UniqueStatement Parser::parse_statement() {
  Token token = m_current_tok;
  Token::Type type = token.m_type;

  switch (type) {
    case Token::Type::OPEN_CURLY_BRACE:
      advance();
      return parse_compound_statement();
    case Token::Type::CLOSE_CURLY_BRACE:
      return nullptr;
    case Token::Type::BREAK:
    case Token::Type::CONTINUE:
      advance();
      return parse_control_statement();
    case Token::Type::WHILE:
    case Token::Type::DO:
      advance();
      return parse_while_statement();
    case Token::Type::ELSE:
      error("Expected Expression.");
      return nullptr;
    case Token::Type::FOR:
      advance();
      return parse_for_statement();
    case Token::Type::IF:
      advance();
      return parse_if_statement();
    case Token::Type::RETURN:
      advance();
      return parse_return_statement();
    case Token::Type::SWITCH:
      advance();
      return parse_switch_statement();
    case Token::Type::GOTO:
      fatal_error("Not Yet Implemented");
    case Token::Type::CASE:
      fatal_error("Not Yet Implemented");
    case Token::Type::DEFAULT:
      fatal_error("Not Yet Implemented");
    case Token::Type::IDENTIFIER:
      if (match(Token::Type::COLON)) {
        return parse_label_statement();
      }
      return parse_expression_statement();
    default:
      fatal_error(std::format("Unexpected Token: {}", token.get_type_string()));
  }
}

Parser::UniqueStatement Parser::parse_if_statement() {
  if (!consume(Token::Type::OPEN_PAREN, R"(Expected "(" after "if")")) {
    return nullptr;
  }

  UniqueExpression condition = parse_expression();

  if (condition == nullptr) {
    error("Expected expression");
    return nullptr;
  }

  if (!consume(Token::Type::CLOSE_PAREN, R"*(Expected ")" after if condition)*")) {
    return nullptr;
  }

  UniqueStatement true_stmt = parse_statement();
  UniqueStatement false_stmt = nullptr;

  if (match(Token::Type::ELSE)) {
    false_stmt = parse_statement();
  }

  IfStmt *stmt = alloc_node<IfStmt>();
  stmt->condition = std::move(condition);
  stmt->true_stmt = std::move(true_stmt);
  stmt->false_stmt = std::move(false_stmt);

  return UniqueStatement(stmt);
}

Parser::UniqueStatement Parser::parse_while_statement() {
  Token token = m_previous_tok;
  WhileStmt::WhileType while_type;
  UniqueExpression condition = nullptr;
  UniqueStatement stmt = nullptr;

  if (token.m_type == Token::Type::DO) {
    while_type = WhileStmt::WhileType::DO;
    stmt = parse_statement();

    if (!consume(Token::Type::WHILE, R"(Expected "while" in "do/while" loop)") ||
        !consume(Token::Type::OPEN_PAREN, R"(Expected "(" after "do/while")")) {
      return nullptr;
    }
    condition = parse_expression();

    if (condition == nullptr) {
      error("Expected expression");
      return nullptr;
    }

    if (!consume(Token::Type::CLOSE_PAREN, R"*(Expected ")")*") ||
        !consume(Token::Type::SEMICOLON, R"(Expected ";")")) {
      return nullptr;
    }
  }
  // Else while
  else {
    while_type = WhileStmt::WhileType::WHILE;

    if (!consume(Token::Type::OPEN_PAREN, R"(Expected "(" after "while")")) {
      return nullptr;
    }
    condition = parse_expression();

    if (condition == nullptr) {
      error("Expected expression");
      return nullptr;
    }

    stmt = parse_statement();
  }

  WhileStmt *while_statement = alloc_node<WhileStmt>();
  while_statement->while_type = while_type;
  while_statement->condition = std::move(condition);
  while_statement->stmt = std::move(stmt);

  return UniqueStatement(while_statement);
}

Parser::UniqueStatement Parser::parse_for_statement() {
  if (!consume(Token::Type::OPEN_PAREN, R"(Expected "(" after "for")")) {
    return nullptr;
  }

  UniqueNode init_clause = parse_decl_stmt();

  UniqueExpression condition = parse_expression();
  if (!consume(Token::Type::SEMICOLON, R"(Expected ";")")) {
    return nullptr;
  }

  UniqueExpression iteration = parse_expression();
  if (!consume(Token::Type::CLOSE_PAREN, R"*(Expected ")")*")) {
    return nullptr;
  }

  ForStmt *for_statement = alloc_node<ForStmt>();
  for_statement->init_clause = std::move(init_clause);
  for_statement->condition = std::move(condition);
  for_statement->iteration = std::move(iteration);

  push_type_context();
  for_statement->stmt = parse_statement();
  pop_type_context();

  return UniqueStatement(for_statement);
}

Parser::UniqueStatement Parser::parse_switch_statement() {
  if (!consume(Token::Type::OPEN_PAREN, R"(Expected "(" after "switch")")) {
    return nullptr;
  }

  UniqueExpression expr = parse_expression();
  if (expr == nullptr) {
    error("Expected expression");
    return nullptr;
  }

  if (!consume(Token::Type::CLOSE_PAREN, R"*(Expected ")")*")) {
    return nullptr;
  }

  SwitchStmt *stmt = alloc_node<SwitchStmt>();
  stmt->expr = std::move(expr);
  stmt->stmt = parse_statement();

  return UniqueStatement(stmt);
}

Parser::UniqueStatement Parser::parse_return_statement() {
  UniqueExpression expr = parse_expression();

  if (!consume(Token::Type::SEMICOLON, R"(Expected ";")")) {
    return nullptr;
  }

  ReturnStmt *stmt = alloc_node<ReturnStmt>();
  stmt->return_value = std::move(expr);

  return UniqueStatement(stmt);
}

Parser::UniqueStatement Parser::parse_label_statement() {
  fatal_error("Not Yet Implemented");
  return nullptr;
}

Parser::UniqueStatement Parser::parse_control_statement() {
  ControlStmt *stmt = alloc_node<ControlStmt>();
  stmt->control = m_previous_tok.m_type == Token::Type::BREAK ? ControlStmt::ControlType::BREAK
                                                              : ControlStmt::ControlType::CONTINUE;

  return UniqueStatement(stmt);
}

Parser::UniqueStatement Parser::parse_expression_statement() {
  ExpressionStmt *stmt = alloc_node<ExpressionStmt>();
  stmt->expr = parse_expression();

  consume(Token::Type::SEMICOLON, R"(Expected ";" after expression)");

  return UniqueStatement(stmt);
}

Parser::UniqueStatement Parser::parse_compound_statement() {
  UniqueNode node = parse_decl_stmt();

  CompoundStmt *compound_statement = alloc_node<CompoundStmt>();

  while (node != nullptr) {
    if (m_panic_mode) {
      synchronize();
    }
    compound_statement->stmts.push_back(std::move(node));
    node = parse_decl_stmt();
  }

  UniqueStatement stmt = UniqueStatement(compound_statement);

  if (!consume(Token::Type::CLOSE_CURLY_BRACE, R"(Expected "}")")) {
    return nullptr;
  }

  return stmt;
}

// Declaration Parsing

bool Parser::is_declaration_start() {
  Token::Type type = m_current_tok.m_type;

  switch (type) {
    case Token::Type::AUTO:
    case Token::Type::CHAR:
    case Token::Type::CONST:
    case Token::Type::DOUBLE:
    case Token::Type::ENUM:
    case Token::Type::EXTERN:
    case Token::Type::FLOAT:
    case Token::Type::INLINE:
    case Token::Type::INT:
    case Token::Type::LONG:
    case Token::Type::SHORT:
    case Token::Type::SIGNED:
    case Token::Type::STATIC:
    case Token::Type::STRUCT:
    case Token::Type::TYPEDEF:
    case Token::Type::UNION:
    case Token::Type::UNSIGNED:
    case Token::Type::VOID:
    case Token::Type::VOLATILE:
      return true;
    case Token::Type::IDENTIFIER:
      return (get_type(m_current_tok.get_val<std::string>()) != nullptr);
    default:
      return false;
  }
}

Parser::UniqueDeclaration Parser::parse_declaration() {
  DeclarationNode::StorageClassSpec spec;
  SharedDataType data_type = parse_declaration_specifiers(spec);
  UniqueDeclaration declarator = parse_declarator(data_type);

  if (declarator == nullptr) {
    return nullptr;
  }

  // TODO declarator lists
  if (match(Token::Type::COMMA)) {
    error("Declarator lists not yet implemented");
    return nullptr;
  }

  declarator->m_storage_spec = spec;
  return declarator;
}

Parser::DeclarationNode::StorageClassSpec Parser::get_storage_spec() {
  Token::Type type = m_current_tok.m_type;

  switch (type) {
    case Token::Type::AUTO:
      warning(R"(Storage class specifier "auto" unsupported)");
      advance();
      return DeclarationNode::StorageClassSpec::AUTO;
    // TODO Register type missing
    case Token::Type::STATIC:
      advance();
      return DeclarationNode::StorageClassSpec::STATIC;
    case Token::Type::EXTERN:
      advance();
      return DeclarationNode::StorageClassSpec::EXTERN;
    case Token::Type::TYPEDEF:
      advance();
      return DeclarationNode::StorageClassSpec::TYPEDEF;
    default:
      return DeclarationNode::StorageClassSpec::NONE;
  }
}

Parser::TypeQualifier Parser::get_type_qualifier() {
  Token::Type type = m_current_tok.m_type;

  switch (type) {
    case Token::Type::CONST:
      advance();
      return TypeQualifier::CONST;
    case Token::Type::VOLATILE:
      advance();
      return TypeQualifier::VOLATILE;
    // TODO Restrict keyword missing
    default:
      return TypeQualifier::NONE;
  }
}

Token Parser::get_type_specifier() {
  Token::Type type = m_current_tok.m_type;

  switch (type) {
    case Token::Type::CHAR:
    case Token::Type::SHORT:
    case Token::Type::INT:
    case Token::Type::LONG:
    case Token::Type::FLOAT:
    case Token::Type::DOUBLE:
    case Token::Type::VOID:
    case Token::Type::SIGNED:
    case Token::Type::UNSIGNED:
    case Token::Type::STRUCT:
    case Token::Type::ENUM:
    case Token::Type::UNION:
      return advance();
    case Token::Type::IDENTIFIER:
      if (get_type(m_current_tok.get_val<std::string>()) != nullptr) {
        Token typedef_name = advance();
        typedef_name.m_type = Token::Type::TYPEDEF_NAME;
        return typedef_name;
      }
    default:
      return m_current_tok;
  }
}

Parser::SharedDataType Parser::parse_type_specifiers(std::vector<Token> &p_type_specifiers) {
  if (p_type_specifiers.empty()) {
    error("Declaration requires type specifier");
    return nullptr;
  }

  enum class Sign { NONE, UNSIGNED, SIGNED } sign = Sign::NONE;

  int long_count = 0;
  int short_count = 0;
  std::string name;

  enum class BaseType { NONE, VOID, CHAR, INT, FLOAT, DOUBLE, STRUCT, ENUM, UNION, TYPEDEF_NAME } base = BaseType::NONE;

#define set_basetype(new_type)                                                                                   \
  do {                                                                                                           \
    if (base != BaseType::NONE) {                                                                                \
      error(std::format(R"(Cannot combine "{}" with previous declaration specifier)", token.get_type_string())); \
      return nullptr;                                                                                            \
    }                                                                                                            \
    base = new_type;                                                                                             \
  } while (0)

  for (Token &token : p_type_specifiers) {
    switch (token.m_type) {
      case Token::Type::VOID:
        set_basetype(BaseType::VOID);
        break;
      case Token::Type::CHAR:
        set_basetype(BaseType::CHAR);
        break;
      case Token::Type::INT:
        set_basetype(BaseType::INT);
        break;
      case Token::Type::FLOAT:
        set_basetype(BaseType::FLOAT);
        break;
      case Token::Type::DOUBLE:
        set_basetype(BaseType::DOUBLE);
        break;
      case Token::Type::STRUCT:
        set_basetype(BaseType::STRUCT);
        break;
      case Token::Type::ENUM:
        set_basetype(BaseType::ENUM);
        break;
      case Token::Type::UNION:
        set_basetype(BaseType::UNION);
        break;
      case Token::Type::TYPEDEF_NAME:
        set_basetype(BaseType::TYPEDEF_NAME);
        name = token.get_val<std::string>();
        break;
      case Token::Type::SIGNED:
        if (sign != Sign::NONE && sign != Sign::SIGNED) {
          error(std::format(R"(Cannot combine "{}" with previous declaration specifier)", token.get_type_string()));
          return nullptr;
        }
        else {
          warning(std::format(R"(Duplicate "{}" declaration specifier)", token.get_type_string()));
        }
        sign = Sign::SIGNED;
      case Token::Type::UNSIGNED:
        if (sign != Sign::NONE && sign != Sign::UNSIGNED) {
          error(std::format(R"(Cannot combine "{}" with previous declaration specifier)", token.get_type_string()));
          return nullptr;
        }
        else {
          warning(std::format(R"(Duplicate "{}" declaration specifier)", token.get_type_string()));
        }
        sign = Sign::UNSIGNED;
      case Token::Type::LONG:
        long_count++;
        break;
      case Token::Type::SHORT:
        short_count++;
        break;
      default:
        break;
    }
  }
#undef check_basetype

  if (long_count > 0 && short_count > 0) {
    error("Cannot use both long and short in declaration");
    return nullptr;
  }

  if ((long_count > 0 || short_count > 0 || sign != Sign::NONE) && base == BaseType::NONE) {
    base = BaseType::INT;
  }

  // Int is only base type than can be long or short
  // TODO double can also be specified as long
  if ((long_count > 0 || short_count > 0) && base != BaseType::INT) {
    error("Invalid declaration specifier usage");
    return nullptr;
  }

  // void, Union, Struct, Enum cannot have sign specifier
  if (sign != Sign::NONE && base != BaseType::CHAR && base != BaseType::INT && base != BaseType::FLOAT &&
      base != BaseType::DOUBLE) {
    error("Invalid declaration specifier usage");
    return nullptr;
  }

  std::string sign_name = sign != Sign::UNSIGNED ? std::string("signed ") : std::string("unsigned ");

  switch (base) {
    case BaseType::CHAR:
      return get_type(sign_name + "char");
    case BaseType::INT:
      if (short_count > 0) {
        return get_type(sign_name + "short");
      }
      if (long_count > 0) {
        return get_type(sign_name + "long");
      }
      return get_type(sign_name + "int");
    case BaseType::FLOAT:
      return get_type(sign_name + "float");
    case BaseType::DOUBLE:
      return get_type(sign_name + "double");
    case BaseType::VOID:
      return get_type("void");
    case BaseType::TYPEDEF_NAME:
      return get_type(name);
    case BaseType::STRUCT:
      return parse_struct_union(DataType::TypeKind::TYPE_STRUCT);
    case BaseType::UNION:
      return parse_struct_union(DataType::TypeKind::TYPE_UNION);
    case BaseType::ENUM:
      return parse_enum();
    case BaseType::NONE:
    default:
      error("Type specifier missing");
      return nullptr;
  }
}

Parser::SharedDataType Parser::parse_struct_union(Parser::DataType::TypeKind p_kind) {
  std::string kind_name = p_kind == Parser::DataType::TypeKind::TYPE_STRUCT ? "struct " : "union ";
  std::string struct_name = "";

  Token token = advance();
  if (token == Token::Type::IDENTIFIER) {
    struct_name = token.get_val<std::string>();
    token = advance();
  }

  std::shared_ptr<StructUnionType> shared_struct = std::make_shared<StructUnionType>();
  if (!struct_name.empty()) {
    SharedDataType data_type = get_type_cur_context(kind_name + struct_name);

    if (token.m_type == Token::Type::OPEN_CURLY_BRACE && data_type != nullptr) {
      if (data_type->m_kind != DataType::TypeKind::TYPE_INCOMPLETE) {
        error(std::format(R"(redefinition of "{}")", struct_name));
        return nullptr;
      }
      shared_struct = std::dynamic_pointer_cast<StructUnionType>(data_type);
    }
    else if (data_type != nullptr) {
      return data_type;
    }
    else {
      push_type(kind_name + struct_name, shared_struct);
      if (token != Token::Type::OPEN_CURLY_BRACE) {
        return shared_struct;
      }
    }
  }

  if (!consume(Token::Type::OPEN_CURLY_BRACE, R"(Expected "identifier" or "{")")) {
    return nullptr;
  }

  token = advance();

  while (!is_at_end() && token != Token::Type::CLOSE_CURLY_BRACE) {
    // TODO parse declarations inside of struct/union
    fatal_error("Not Yet Implemented.");
  }

  consume(Token::Type::CLOSE_CURLY_BRACE, R"(Expected "}")");

  shared_struct->m_kind = p_kind;

  return shared_struct;
}

Parser::SharedDataType Parser::parse_enum() {
  std::string enum_name = "";

  Token token = advance();
  if (token == Token::Type::IDENTIFIER) {
    enum_name = token.get_val<std::string>();
    token = advance();
  }

  SharedDataType shared_enum = SharedDataType(new EnumType());
  if (!enum_name.empty()) {
    SharedDataType data_type = get_type_cur_context("enum " + enum_name);
    if (token.m_type == Token::Type::OPEN_CURLY_BRACE && data_type != nullptr) {
      if (data_type->m_kind != DataType::TypeKind::TYPE_INCOMPLETE) {
        error(std::format(R"(redefinition of "{}")", enum_name));
        return nullptr;
      }
      shared_enum = data_type;
    }
    else if (data_type != nullptr) {
      return data_type;
    }
    else {
      push_type("enum " + enum_name, shared_enum);
      if (token != Token::Type::OPEN_CURLY_BRACE) {
        shared_enum->m_kind = DataType::TypeKind::TYPE_INCOMPLETE;
        return shared_enum;
      }
    }
  }

  if (token != Token::Type::OPEN_CURLY_BRACE) {
    error(R"(Expected "identifier" or "{")");
    return nullptr;
  }
  shared_enum->m_kind = DataType::TypeKind::TYPE_ENUM;

  token = advance();

  int value = 0;
  while (!is_at_end() && token != Token::Type::CLOSE_CURLY_BRACE) {
    if (token != Token::Type::IDENTIFIER) {
      error("Expected Identifier");
      return nullptr;
    }

    std::string name = token.get_val<std::string>();
    token = advance();
    if (token == Token::Type::COMMA) {
      token = advance();
    }
    else if (token == Token::Type::EQUAL) {
      token = advance();

      // TODO expressions that can be evaluated at compile time are allowed
      if (token == Token::Type::IDENTIFIER) {
        std::optional<int> opt_val = get_enum_val(token.get_val<std::string>());
        if (!opt_val.has_value()) {
          error("expression is not an integer constant expression");
          return nullptr;
        }
        value = *opt_val;
      }
      else if (token == Token::Type::NUMBER_LITERAL) {
        value = token.get_val<uint64_t>();
      }
      token = advance();
    }

    if (get_enum_val(name).has_value()) {
      error(std::format(R"(redefinition of enumerator "{}")", name));
      m_panic_mode = false;
      continue;
    }

    set_enum_val(name, value++);
  }

  consume(Token::Type::CLOSE_CURLY_BRACE, R"(Expected "}")");

  return shared_enum;
}

Parser::SharedDataType Parser::parse_declaration_specifiers(DeclarationNode::StorageClassSpec &p_spec) {
  p_spec = DeclarationNode::StorageClassSpec::NONE;
  std::unordered_set<TypeQualifier> qualifiers;
  std::vector<Token> type_specifiers;

  while (is_declaration_start()) {
    DeclarationNode::StorageClassSpec temp_spec = get_storage_spec();
    if (p_spec != DeclarationNode::StorageClassSpec::NONE && temp_spec != DeclarationNode::StorageClassSpec::NONE) {
      if (p_spec == temp_spec) {
        warning(std::format(R"(Duplicate "{}" declaration specifier)", m_previous_tok.get_type_string()));
      }
      else {
        error(std::format(R"(Storage class specifier "{}" incompatible with previous specifier)",
                          m_previous_tok.get_type_string()));
        return nullptr;
      }
    }
    else if (p_spec == DeclarationNode::StorageClassSpec::NONE) {
      p_spec = temp_spec;
    }

    TypeQualifier temp_qualifier = get_type_qualifier();
    if (temp_qualifier != TypeQualifier::NONE) {
      if (qualifiers.contains(temp_qualifier)) {
        warning(std::format(R"(Duplicate "{}" type qualifier)", m_previous_tok.get_type_string()));
      }
      qualifiers.insert(temp_qualifier);
    }

    Token temp_type_spec = get_type_specifier();

    switch (temp_type_spec.m_type) {
      case Token::Type::CHAR:
      case Token::Type::SHORT:
      case Token::Type::INT:
      case Token::Type::LONG:
      case Token::Type::FLOAT:
      case Token::Type::DOUBLE:
      case Token::Type::VOID:
      case Token::Type::SIGNED:
      case Token::Type::UNSIGNED:
      case Token::Type::STRUCT:
      case Token::Type::ENUM:
      case Token::Type::UNION:
      case Token::Type::TYPEDEF_NAME:
        type_specifiers.push_back(temp_type_spec);
        break;
      default:
        break;
    }
  }

  SharedDataType data_type = nullptr;
  // C99 spec mentions that if restrict is used with no type the data_type is int
  if (type_specifiers.empty() && qualifiers.contains(TypeQualifier::RESTRICT)) {
    data_type = get_type("signed int");
  }
  else {
    data_type = parse_type_specifiers(type_specifiers);
  }

  if (data_type == nullptr) {
    error("A type specifier is required for all declarations");
    return nullptr;
  }

  data_type = data_type->clone();
  data_type->set_qualifiers(qualifiers);
  return data_type;
}

Parser::UniqueDeclaration Parser::parse_declarator(Parser::SharedDataType p_base_type) {
  p_base_type = parse_pointer(p_base_type);

  // TODO complex declarators
  if (match(Token::Type::OPEN_PAREN)) {
    error("Complex declarators not yet implemented");
    return nullptr;
  }

  if (!consume(Token::Type::IDENTIFIER, "Expected Identifier")) {
    return nullptr;
  }

  std::string identifier = m_previous_tok.get_val<std::string>();

  if (match(Token::Type::OPEN_PAREN)) {
    return parse_function(p_base_type, identifier);
  }
  else if (match(Token::Type::OPEN_SQUARE_BRACKET)) {
    return parse_array(p_base_type, identifier);
  }
  else if (match(Token::Type::EQUAL)) {
    // TODO initializer list
    UniqueExpression init = parse_precedence(Precedence::PREC_ASSIGNMENT);
    if (init == nullptr) {
      error("Expected expression");
      return nullptr;
    }

    VariableDeclaration *decl = alloc_node<VariableDeclaration>();
    decl->m_data_type = p_base_type;
    decl->initializer = std::move(init);
    decl->identifier = identifier;

    return UniqueDeclaration(decl);
  }
  else {
    VariableDeclaration *decl = alloc_node<VariableDeclaration>();
    decl->m_data_type = p_base_type;
    decl->identifier = identifier;
    return UniqueDeclaration(decl);
  }
}

Parser::SharedDataType Parser::parse_pointer(Parser::SharedDataType p_base_type) {
  while (match(Token::Type::STAR)) {
    std::unordered_set<TypeQualifier> qualifiers;
    TypeQualifier qualifier = get_type_qualifier();
    while (qualifier != TypeQualifier::NONE) {
      qualifiers.insert(qualifier);
      qualifier = get_type_qualifier();
    }

    PointerType *ptr_type = new PointerType();
    ptr_type->set_qualifiers(qualifiers);
    ptr_type->m_base_type = p_base_type;
    p_base_type = SharedDataType(ptr_type);
  }

  return p_base_type;
}

Parser::UniqueDeclaration Parser::parse_function(Parser::SharedDataType p_return_type,
                                                 const std::string &p_identifier) {
  std::shared_ptr<FunctionType> function_type = std::make_shared<FunctionType>();
  function_type->return_type = p_return_type;
  std::vector<UniqueDeclaration> params;

  while (!is_at_end() && !match(Token::Type::CLOSE_PAREN)) {
    DeclarationNode::StorageClassSpec spec;
    SharedDataType data_type = parse_declaration_specifiers(spec);
    UniqueDeclaration param = parse_declarator(data_type);
    param->m_storage_spec = spec;

    function_type->param_types.push_back(param->m_data_type);
    params.push_back(std::move(param));

    if (!check(Token::Type::COMMA) && !check(Token::Type::CLOSE_PAREN)) {
      error(R"*(Expected "," or ")")*");
      return nullptr;
    }

    // Happens when we see this sequence    ,)
    if (match(Token::Type::COMMA) && match(Token::Type::CLOSE_PAREN)) {
      error("Expected parameter declarator");
      break;
    }
  }

  if (match(Token::Type::SEMICOLON)) {
    FunctionDefinition *decl = alloc_node<FunctionDefinition>();
    decl->m_data_type = function_type;
    decl->identifier = p_identifier;
    decl->params = std::move(params);

    return UniqueDeclaration(decl);
  }
  if (match(Token::Type::OPEN_CURLY_BRACE)) {
    UniqueStatement stmt = parse_compound_statement();
    if (stmt == nullptr) {
      error("Expected function body after function declarator");
      return nullptr;
    }

    FunctionDefinition *decl = alloc_node<FunctionDefinition>();
    decl->m_data_type = function_type;
    decl->identifier = p_identifier;
    decl->params = std::move(params);
    decl->body = std::move(stmt);

    return UniqueDeclaration(decl);
  }

  return nullptr;
}

Parser::UniqueDeclaration Parser::parse_array(Parser::SharedDataType p_base_type, const std::string &p_identifier) {
  UniqueExpression expr = nullptr;
  if (!match(Token::Type::CLOSE_SQUARE_BRACKET)) {
    expr = parse_precedence(Precedence::PREC_ASSIGNMENT);
    if (expr == nullptr) {
      error("Expected expression");
      return nullptr;
    }
  }

  if (!consume(Token::Type::CLOSE_SQUARE_BRACKET, R"(Expected "]")")) {
    return nullptr;
  }
  // TODO initalizer list
  if (match(Token::Type::EQUAL)) {
    error("Array Initialization not yet implemented");
    return nullptr;
  }

  ArrayType *array_type = new ArrayType();
  array_type->m_base_type = p_base_type;

  ArrayDeclaration *decl = alloc_node<ArrayDeclaration>();
  decl->m_data_type = SharedDataType(array_type);
  decl->identifier = p_identifier;

  decl->size = std::move(expr);

  return UniqueDeclaration(decl);
}

// Program parsing

Parser::UniqueNode Parser::parse_decl_stmt() {
  if (!is_declaration_start()) {
    UniqueStatement stmt = parse_statement();
    return UniqueNode(dynamic_cast<Node *>(stmt.release()));
  }
  UniqueDeclaration decl = parse_declaration();
  if (!consume(Token::Type::SEMICOLON, R"(Expected ";")")) {
    return nullptr;
  }
  if (decl != nullptr) {
    return UniqueNode(dynamic_cast<Node *>(decl.release()));
  }

  return nullptr;
}

// Parser::UniqueDeclaration

// Parsing Helpers

Token Parser::advance() {
  if (m_token_index >= m_tokens.size()) {
    throw std::runtime_error("Parser: Trying to advance past end of token stream.");
  }

  m_previous_tok = m_current_tok;
  m_current_tok = m_tokens[m_token_index++];

  return m_previous_tok;
}

bool Parser::match(Token::Type p_token_type) {
  if (!check(p_token_type)) {
    return false;
  }
  advance();
  return true;
}

bool Parser::check(Token::Type p_token_type) const { return m_current_tok == p_token_type; }

bool Parser::consume(Token::Type p_token_type, const std::string &p_error_message) {
  if (match(p_token_type)) {
    return true;
  }
  error(p_error_message);
  return false;
}

void Parser::synchronize() {
  m_panic_mode = false;
  while (!is_at_end()) {
    if (m_previous_tok.m_type == Token::Type::SEMICOLON) {
      return;
    }

    switch (m_current_tok.m_type) {
      case Token::Type::AUTO:
      case Token::Type::BREAK:
      case Token::Type::CASE:
      case Token::Type::CHAR:
      case Token::Type::CONST:
      case Token::Type::CONTINUE:
      case Token::Type::DEFAULT:
      case Token::Type::DO:
      case Token::Type::DOUBLE:
      case Token::Type::ELSE:
      case Token::Type::ENUM:
      case Token::Type::EXTERN:
      case Token::Type::FLOAT:
      case Token::Type::FOR:
      case Token::Type::GOTO:
      case Token::Type::IF:
      case Token::Type::INLINE:
      case Token::Type::INT:
      case Token::Type::LONG:
      case Token::Type::RETURN:
      case Token::Type::SHORT:
      case Token::Type::SIGNED:
      case Token::Type::SIZEOF:
      case Token::Type::STATIC:
      case Token::Type::STRUCT:
      case Token::Type::SWITCH:
      case Token::Type::TYPEDEF:
      case Token::Type::UNION:
      case Token::Type::UNSIGNED:
      case Token::Type::VOID:
      case Token::Type::VOLATILE:
      case Token::Type::WHILE:
        return;
      default:
        break;
    }
    advance();
  }
}

}  // namespace JCC

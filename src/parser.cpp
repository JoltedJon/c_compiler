#include "parser.hpp"

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
#include "ast.hpp"
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

  register_base_type("float", DataType::TypeKind::TYPE_FLOAT, true, 4);
  register_base_type("double", DataType::TypeKind::TYPE_FLOAT, true, 8);

  register_base_type("void", DataType::TypeKind::TYPE_VOID, false, 0);

  register_base_type("unresolved type", DataType::TypeKind::TYPE_UNRESOLVED, false, 0);
}

UniqueNode Parser::parse() {
  std::vector<UniqueDeclaration> program;

  while (!is_at_end()) {
    program.push_back(parse_declaration());
    if (m_panic_mode) {
      synchronize();
    }
  }

  TranslationUnit *unit = alloc_node<TranslationUnit>();
  unit->m_program = std::move(program);

  return UniqueNode(unit);
}

Parser::ParseRule *Parser::get_rule(Token::Type p_token_type) {
  static ParseRule rules[] = {
      // prefix   infix   precedence
      // Punctuation
      {&Parser::parse_grouping, &Parser::parse_call, Precedence::PREC_CALL},  //     LEFT_PAREN
      {nullptr, nullptr, Precedence::PREC_NONE},                              //     RIGHT_PAREN
      {nullptr, nullptr, Precedence::PREC_NONE},                              //     LEFT_BRACE {
      {nullptr, nullptr, Precedence::PREC_NONE},                              //     RIGHT_BRACE }
      {nullptr, &Parser::parse_subscript, Precedence::PREC_CALL},             //     LEFT_BRACKET [
      {nullptr, nullptr, Precedence::PREC_NONE},                              //     RIGHT_BRACKET ]
      {nullptr, &Parser::parse_member_access, Precedence::PREC_CALL},         //     DOT
      {nullptr, &Parser::parse_member_access, Precedence::PREC_CALL},         //     ARROW
      {nullptr, nullptr, Precedence::PREC_NONE},                              //     SEMICOLON

      {&Parser::parse_unary_operation, nullptr, Precedence::PREC_UNARY},                             //     TILDE
      {&Parser::parse_unary_operation, nullptr, Precedence::PREC_UNARY},                             //     BANG
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, Precedence::PREC_ADDITION},  //     PLUS
      {&Parser::parse_unary_operation, &Parser::parse_postfix, Precedence::PREC_CALL},               //     PLUS_PLUS
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, Precedence::PREC_ADDITION},  //     MINUS
      {&Parser::parse_unary_operation, &Parser::parse_postfix, Precedence::PREC_CALL},               //     MINUS_MINUS
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, Precedence::PREC_FACTOR},    //     STAR
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_FACTOR},                           //     SLASH
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_FACTOR},                           //     PERCENT

      // Equality
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_EQUALITY},  //     BANG_EQUAL
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_EQUALITY},  //     EQUAL_EQUAL

      // Comparision
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_RELATIONAL},  //     GREATER
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_RELATIONAL},  //     GREATER_EQUAL
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_RELATIONAL},  //     LESS
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_RELATIONAL},  //     LESS_EQUAL

      // Bitwise
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_BIT_SHIFT},  //     SHIFT_RIGHT
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_BIT_SHIFT},  //     SHIFT_LEFT

      // Logic and Bitwise
      {&Parser::parse_unary_operation, &Parser::parse_binary_operation, Precedence::PREC_BIT_AND},  //     AMPERSAND
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_BIT_XOR},                         //     CARET
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_BIT_OR},                          //     PIPE
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_LOGIC_AND},  //     AMPERSAND_AMPERSAND
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_LOGIC_OR},   //     PIPE_PIPE

      // Ternary
      {nullptr, nullptr, Precedence::PREC_TERNARY},                          //     COLON
      {nullptr, &Parser::parse_ternary_operator, Precedence::PREC_TERNARY},  //     QUESTION

      // Assignment
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     PLUS_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     MINUS_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     STAR_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     SLASH_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     PERCENT_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     AMPERSAND_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     PIPE_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     CARET_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     SHIFT_RIGHT_EQUAL
      {nullptr, &Parser::parse_assignment_operation, Precedence::PREC_ASSIGNMENT},  //     SHIFT_LEFT_EQUAL

      // Comma
      {nullptr, &Parser::parse_binary_operation, Precedence::PREC_COMMA},  //     COMMA

      // Literals
      {&Parser::parse_identifier, nullptr, Precedence::PREC_NONE},  //     IDENTIFIER
      {&Parser::parse_literal, nullptr, Precedence::PREC_NONE},     //     STRING_LITERAL
      {&Parser::parse_literal, nullptr, Precedence::PREC_NONE},     //     NUMBER_LITERAL
      {&Parser::parse_literal, nullptr, Precedence::PREC_NONE},     //     FLOAT_LITERAL

      // Keywords
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     AUTO
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     BREAK
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     CASE
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     CHAR
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     CONST
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     CONTINUE
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     DEFAULT
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     DO
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     DOUBLE
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     ELSE
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     ENUM
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     EXTERN
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     FLOAT
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     FOR
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     GOTO
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     IF
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     INLINE
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     INT
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     LONG
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     RETURN
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     SHORT
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     SIGNED
      {&Parser::parse_unary_operation, nullptr, Precedence::PREC_NONE},  //     SIZEOF
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     STATIC
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     STRUCT
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     SWITCH
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     TYPEDEF
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     UNION
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     UNSIGNED
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     VOID
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     VOLATILE
      {nullptr, nullptr, Precedence::PREC_NONE},                         //     WHILE

      // Macros
      {nullptr, nullptr, Precedence::PREC_NONE},  //     NEWLINE
      {nullptr, nullptr, Precedence::PREC_NONE},  //     DEFINE
      {nullptr, nullptr, Precedence::PREC_NONE},  //     UNDEFINE
      {nullptr, nullptr, Precedence::PREC_NONE},  //     IF_DEFINED
      {nullptr, nullptr, Precedence::PREC_NONE},  //     IF_NOT_DEFINED
      {nullptr, nullptr, Precedence::PREC_NONE},  //     END_IF
      {nullptr, nullptr, Precedence::PREC_NONE},  //     ELLIPSIS,
      {nullptr, nullptr, Precedence::PREC_NONE},  //     INCLUDE
      {nullptr, nullptr, Precedence::PREC_NONE},  //     ERROR

      {nullptr, nullptr, Precedence::PREC_NONE},  //     TYPEDEF_NAME
  };

  static_assert(sizeof(rules) / sizeof(rules[0]) == static_cast<int>(Token::Type::EOFF),
                "Amount of parse rules does not match the amount of token types.");

  return &rules[p_token_type];
}

UniqueExpression Parser::parse_expression() { return parse_precedence(Precedence::PREC_COMMA); }

UniqueExpression Parser::parse_precedence(Parser::Precedence p_precedence) {
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
    if (previous_operand == nullptr || check(Token::Type::COLON)) {
      return previous_operand;
    }

    token = advance();
    ParseFunction infix_rule = get_rule(token.m_type)->infix;
    previous_operand = (this->*infix_rule)(std::move(previous_operand));
  }

  return previous_operand;
}

UniqueExpression Parser::parse_grouping(UniqueExpression p_previous_operand) {
  if (is_declaration_start()) {
    // TODO cast cannot have storage class spec but parse_declaration_specifier will parse it anyways
    DeclarationNode::StorageClassSpec p_spec;
    SharedDataType cast_type = parse_declaration_specifiers(p_spec);

    consume(Token::Type::CLOSE_PAREN, R"-(Expected ")".)-");
    UniqueExpression expr = parse_expression();

    if (expr == nullptr) {
      error("expected expression");
      return nullptr;
    }

    UnaryOpNode *cast_expr = alloc_node<UnaryOpNode>();
    cast_expr->m_data_type = std::move(cast_type);
    cast_expr->m_operand = std::move(expr);
    cast_expr->m_operation = UnaryOpNode::OpType::OP_CAST;

    return UniqueExpression(cast_expr);
  }

  UniqueExpression expr = parse_expression();

  consume(Token::Type::CLOSE_PAREN, R"-(Expected ")".)-");

  return expr;
}

UniqueExpression Parser::parse_call(UniqueExpression p_previous_operand) {
  CallNode *call = alloc_node<CallNode>();

  call->m_callee = std::move(p_previous_operand);

  if (call->m_callee == nullptr) {
    error("Expected Expression on call.");
  }
  else if (call->m_callee->m_node_type == Node::NodeType::IDENTIFIER) {
    call->m_name = dynamic_cast<IdentifierNode &>(*call->m_callee).m_name;
  }

  do {
    if (check(Token::Type::CLOSE_PAREN)) {
      break;
    }
    UniqueExpression argument = parse_precedence(Precedence::PREC_ASSIGNMENT);

    if (argument == nullptr) {
      error("Expected Expression as function argument.");
    }
    else {
      call->m_args.push_back(std::move(argument));
    }
  } while (match(Token::Type::COMMA));

  consume(Token::Type::CLOSE_PAREN, R"*(Expected closing ")" after function call arguments.)*");

  call->m_data_type = get_type("unresolved type");

  return UniqueExpression(call);
}

UniqueExpression Parser::parse_subscript(UniqueExpression p_previous_operand) {
  BinaryOpNode *operation = alloc_node<BinaryOpNode>();
  operation->m_operation = BinaryOpNode::OpType::OP_ARRAY_SUBSCRIPT;
  operation->m_left_operand = std::move(p_previous_operand);
  operation->m_right_operand = parse_expression();

  consume(Token::Type::CLOSE_SQUARE_BRACKET, R"(Expected "]" after array subscript expression.)");

  operation->m_data_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

UniqueExpression Parser::parse_binary_operation(UniqueExpression p_previous_operand) {
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
      {Token::Type::COMMA, BinaryOpNode::OpType::OP_COMMA}};

  Token op = m_previous_tok;
  BinaryOpNode *operation = alloc_node<BinaryOpNode>();

  Precedence precedence = (Precedence)(get_rule(op.m_type)->precedence + 1);
  operation->m_left_operand = std::move(p_previous_operand);
  operation->m_right_operand = parse_precedence(precedence);

  if (operation->m_right_operand == nullptr) {
    error(std::format(R"(Expected expression after "{}" operator.)", op.get_type_string()));
  }

  auto it = binaryOps.find(op.m_type);

  if (it == binaryOps.end()) {
    fatal_error(std::format("Unexpected Token: {}", op.get_type_string()));
  }

  operation->m_operation = it->second;
  operation->m_data_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

UniqueExpression Parser::parse_assignment_operation(UniqueExpression p_previous_operand) {
  static std::unordered_map<Token::Type, BinaryOpNode::OpType> assignOps = {
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
      {Token::Type::SHIFT_LEFT_EQUAL, BinaryOpNode::OpType::OP_RIGHT_SHIFT_ASSIGN},
  };

  Token op = m_previous_tok;
  BinaryOpNode *operation = alloc_node<BinaryOpNode>();

  Precedence precedence = (Precedence)(get_rule(op.m_type)->precedence);
  operation->m_left_operand = std::move(p_previous_operand);
  operation->m_right_operand = parse_precedence(precedence);

  if (operation->m_right_operand == nullptr) {
    error(std::format(R"(Expected expression after "{}" operator.)", op.get_type_string()));
  }

  auto it = assignOps.find(op.m_type);

  if (it == assignOps.end()) {
    fatal_error(std::format("Unexpected Token: {}", op.get_type_string()));
  }

  operation->m_operation = it->second;
  operation->m_data_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

UniqueExpression Parser::parse_unary_operation(UniqueExpression p_previous_operand) {
  Token::Type op_type = m_previous_tok.m_type;
  UnaryOpNode *operation = alloc_node<UnaryOpNode>();
  UniqueExpression uniq_op = UniqueExpression(operation);
  operation->m_operand = parse_precedence(Precedence::PREC_UNARY);

  switch (op_type) {
    case Token::Type::PLUS:
      operation->m_operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->m_operand) {
        error(R"(Expected expression after "+" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::MINUS:
      operation->m_operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->m_operand) {
        error(R"(Expected expression after "-" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::PLUS_PLUS:
      operation->m_operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->m_operand) {
        error(R"(Expected expression after "++" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::MINUS_MINUS:
      operation->m_operation = UnaryOpNode::OpType::OP_POSITIVE;
      if (!operation->m_operand) {
        error(R"(Expected expression after "--" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::BANG:
      operation->m_operation = UnaryOpNode::OpType::OP_LOGIC_NOT;
      if (!operation->m_operand) {
        error(R"(Expected expression after "!" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::TILDE:
      operation->m_operation = UnaryOpNode::OpType::OP_LOGIC_NOT;
      if (!operation->m_operand) {
        error(R"(Expected expression after "~" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::AMPERSAND:
      operation->m_operation = UnaryOpNode::OpType::OP_ADDRESS_OF;
      if (!operation->m_operand) {
        error(R"(Expected expression after "&" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::STAR:
      operation->m_operation = UnaryOpNode::OpType::OP_INDIRECTION;
      if (!operation->m_operand) {
        error(R"(Expected expression after "*" operator.)");
        return nullptr;
      }
      break;
    case Token::Type::SIZEOF:
      operation->m_operation = UnaryOpNode::OpType::OP_SIZEOF;
      // TODO Parse this expression
      // sizeof ( type-name )
      break;
    default:
      return nullptr;
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  operation->m_data_type = get_type("unresolved type");

  return uniq_op;
}

UniqueExpression Parser::parse_ternary_operator(UniqueExpression p_previous_operand) {
  TernaryOpNode *operation = alloc_node<TernaryOpNode>();

  operation->m_condition = std::move(p_previous_operand);
  operation->m_true_expr = parse_expression();

  if (operation->m_true_expr == nullptr) {
    error(R"(Expected expression after "?" operator.)");
  }

  consume(Token::Type::COLON, R"(Expected ":".)");

  operation->m_false_expr = parse_precedence(Precedence::PREC_TERNARY);

  if (operation->m_false_expr == nullptr) {
    error(R"(Expected expression after ":".)");
  }

  operation->m_data_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

UniqueExpression Parser::parse_member_access(UniqueExpression p_previous_operand) {
  Token op = m_previous_tok;

  if (!consume(Token::Type::IDENTIFIER, "Expected Identifier")) {
    return nullptr;
  }

  std::string identifier = m_previous_tok.get_val<std::string>();

  MemberAccessNode *operation = alloc_node<MemberAccessNode>();
  operation->m_expr = std::move(p_previous_operand);
  operation->m_member = identifier;

  switch (op.m_type) {
    case Token::Type::DOT:
      operation->m_access_type = MemberAccessNode::OpType::OP_DIRECT_MEM_ACCESS;
      break;
    case Token::Type::ARROW:
      operation->m_access_type = MemberAccessNode::OpType::OP_INDIRECT_MEM_ACCESS;
      break;
    default:
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  operation->m_data_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

UniqueExpression Parser::parse_postfix(UniqueExpression p_previous_operand) {
  Token op = m_previous_tok;

  UnaryOpNode *operation = alloc_node<UnaryOpNode>();
  operation->m_operand = std::move(p_previous_operand);

  switch (op.m_type) {
    case Token::Type::PLUS_PLUS:
      operation->m_operation = UnaryOpNode::OpType::OP_POST_INCREMENT;
      break;
    case Token::Type::MINUS_MINUS:
      operation->m_operation = UnaryOpNode::OpType::OP_POST_DECREMENT;
      break;
    default:
      fatal_error(std::format("Unexpected Token: {}", m_previous_tok.get_type_string()));
  }

  operation->m_data_type = get_type("unresolved type");

  return UniqueExpression(operation);
}

UniqueExpression Parser::parse_identifier(UniqueExpression p_previous_operand) {
  std::optional<int> opt_value = get_enum_val(m_previous_tok.get_val<std::string>());
  if (opt_value.has_value()) {
    ConstantNode *constant = alloc_node<ConstantNode>();
    constant->m_value = *opt_value;
    constant->m_val_type = ConstantNode::ValType::SIGNED_INTEGER;
    constant->m_data_type = get_type("signed int");
    return UniqueExpression(constant);
  }

  IdentifierNode *ident = alloc_node<IdentifierNode>();
  ident->m_name = m_previous_tok.get_val<std::string>();

  if (ident->m_name == "") {
    error("Identifier name is Empty.");
  }

  ident->m_data_type = get_type("unresolved type");

  return UniqueExpression(ident);
}

UniqueExpression Parser::parse_literal(UniqueExpression p_previous_operand) {
  Token lit = m_previous_tok;
  ConstantNode *constant = alloc_node<ConstantNode>();
  constant->m_value = m_previous_tok.m_literal;

  switch (lit.m_type) {
    case Token::Type::NUMBER_LITERAL:
      switch (lit.m_num_type) {
        case Token::NumType::SIGNED_INTEGER:
          constant->m_val_type = ConstantNode::ValType::SIGNED_INTEGER;
          constant->m_data_type = get_type("signed int");
          break;
        case Token::NumType::UNSIGNED_INTEGER:
          constant->m_val_type = ConstantNode::ValType::UNSIGNED_INTEGER;
          constant->m_data_type = get_type("unsigned int");
          break;
        case Token::NumType::SIGNED_LONG:
          constant->m_val_type = ConstantNode::ValType::SIGNED_LONG;
          constant->m_data_type = get_type("signed long");
          break;
        case Token::NumType::UNSIGNED_LONG:
          constant->m_val_type = ConstantNode::ValType::UNSIGNED_LONG;
          constant->m_data_type = get_type("unsigned long");
          break;
        default:
          fatal_error(std::format("Unexpected Token: {}", lit.get_type_string()));
      }
      break;
    case Token::Type::FLOAT_LITERAL:
      constant->m_val_type = ConstantNode::ValType::FLOAT;
      constant->m_data_type = get_type("double");
      break;
    case Token::Type::STRING_LITERAL: {
      constant->m_val_type = ConstantNode::ValType::STRING;
      ArrayType *str_type = new ArrayType;
      str_type->m_base_type = get_type("unsigned char");
      str_type->m_size = constant->get_val<std::string>().size() + 1;
      str_type->m_is_const = true;
      constant->m_data_type = SharedDataType(str_type);
    } break;
    default:
      fatal_error(std::format("Unexpected Token: {}", lit.get_type_string()));
  }

  return UniqueExpression(constant);
}

// Statements
UniqueStatement Parser::parse_statement() {
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
      advance();
      return parse_goto_statement();
    case Token::Type::CASE:
      advance();
      return parse_case_statement();
    case Token::Type::DEFAULT:
      advance();
      return parse_default_statement();
    case Token::Type::IDENTIFIER: {
      std::string identifier = token.get_val<std::string>();
      if (match(Token::Type::COLON)) {
        return parse_label_statement(identifier);
      }
      return parse_expression_statement();
    }
    default:
      fatal_error(std::format("Unexpected Token: {}", token.get_type_string()));
  }
}

UniqueStatement Parser::parse_if_statement() {
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
  stmt->m_condition = std::move(condition);
  stmt->m_true_stmt = std::move(true_stmt);
  stmt->m_false_stmt = std::move(false_stmt);

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_while_statement() {
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
  while_statement->m_while_type = while_type;
  while_statement->m_condition = std::move(condition);
  while_statement->m_stmt = std::move(stmt);

  return UniqueStatement(while_statement);
}

UniqueStatement Parser::parse_for_statement() {
  if (!consume(Token::Type::OPEN_PAREN, R"(Expected "(" after "for")")) {
    return nullptr;
  }

  UniqueNode init_clause = parse_decl_stmt();

  UniqueExpression condition = parse_expression();

  // No condition passed means condition is to be treated as constant non-zero integer
  if (condition == nullptr) {
    ConstantNode *constant = alloc_node<ConstantNode>();
    constant->m_value = static_cast<int32_t>(1);
    constant->m_val_type = ConstantNode::ValType::SIGNED_INTEGER;
    constant->m_data_type = get_type("signed int");
    condition = UniqueExpression(constant);
  }

  if (!consume(Token::Type::SEMICOLON, R"(Expected ";")")) {
    return nullptr;
  }

  UniqueExpression iteration = parse_expression();
  if (!consume(Token::Type::CLOSE_PAREN, R"*(Expected ")")*")) {
    return nullptr;
  }

  ForStmt *for_statement = alloc_node<ForStmt>();
  for_statement->m_init_clause = std::move(init_clause);
  for_statement->m_condition = std::move(condition);
  for_statement->m_iteration = std::move(iteration);

  push_type_context();
  for_statement->m_stmt = parse_statement();
  pop_type_context();

  return UniqueStatement(for_statement);
}

UniqueStatement Parser::parse_switch_statement() {
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
  stmt->m_expr = std::move(expr);
  stmt->m_stmt = parse_statement();

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_return_statement() {
  UniqueExpression expr = parse_expression();

  if (!consume(Token::Type::SEMICOLON, R"(Expected ";")")) {
    return nullptr;
  }

  ReturnStmt *stmt = alloc_node<ReturnStmt>();
  stmt->m_return_value = std::move(expr);

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_label_statement(const std::string &p_label) {
  LabelStmt *stmt = alloc_node<LabelStmt>();
  stmt->m_label = p_label;
  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_goto_statement() {
  if (!consume(Token::Type::IDENTIFIER, "Expected identifier")) {
    return nullptr;
  }

  std::string identifier = m_previous_tok.get_val<std::string>();

  if (!consume(Token::Type::SEMICOLON, R"(Expected ";" after goto statement)")) {
    return nullptr;
  }

  GotoStmt *stmt = alloc_node<GotoStmt>();
  stmt->m_label = std::move(identifier);

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_case_statement() {
  UniqueExpression expr = parse_precedence(Precedence::PREC_TERNARY);

  if (!consume(Token::Type::COLON, R"(Expected ":" after case statement)")) {
    return nullptr;
  }

  CaseStmt *stmt = alloc_node<CaseStmt>();
  stmt->m_expr = std::move(expr);

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_default_statement() {
  if (!consume(Token::Type::COLON, R"(Expected ":" after default statement)")) {
    return nullptr;
  }

  return UniqueStatement(alloc_node<DefaultStmt>());
}

UniqueStatement Parser::parse_control_statement() {
  ControlStmt *stmt = alloc_node<ControlStmt>();
  stmt->m_control = m_previous_tok.m_type == Token::Type::BREAK ? ControlStmt::ControlType::BREAK
                                                                : ControlStmt::ControlType::CONTINUE;

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_expression_statement() {
  ExpressionStmt *stmt = alloc_node<ExpressionStmt>();
  stmt->m_expr = parse_expression();

  consume(Token::Type::SEMICOLON, R"(Expected ";" after expression)");

  return UniqueStatement(stmt);
}

UniqueStatement Parser::parse_compound_statement() {
  UniqueNode node = parse_decl_stmt();

  CompoundStmt *compound_statement = alloc_node<CompoundStmt>();

  while (node != nullptr) {
    if (m_panic_mode) {
      synchronize();
    }
    compound_statement->m_stmts.push_back(std::move(node));
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

UniqueDeclaration Parser::parse_declaration() {
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

DeclarationNode::StorageClassSpec Parser::get_storage_spec() {
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

TypeQualifier Parser::get_type_qualifier() {
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

SharedDataType Parser::parse_type_specifiers(std::vector<Token> &p_type_specifiers) {
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

  // void, Union, Struct, Enum, float, double cannot have sign specifier
  if (sign != Sign::NONE && base != BaseType::CHAR && base != BaseType::INT) {
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
      return get_type("float");
    case BaseType::DOUBLE:
      return get_type("double");
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

SharedDataType Parser::parse_struct_union(DataType::TypeKind p_kind) {
  std::string kind_name = p_kind == DataType::TypeKind::TYPE_STRUCT ? "struct " : "union ";
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

SharedDataType Parser::parse_enum() {
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

SharedDataType Parser::parse_declaration_specifiers(DeclarationNode::StorageClassSpec &p_spec) {
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

UniqueDeclaration Parser::parse_declarator(SharedDataType p_base_type) {
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
    decl->m_initializer = std::move(init);
    decl->m_identifier = identifier;

    return UniqueDeclaration(decl);
  }
  else {
    VariableDeclaration *decl = alloc_node<VariableDeclaration>();
    decl->m_data_type = p_base_type;
    decl->m_identifier = identifier;
    return UniqueDeclaration(decl);
  }
}

SharedDataType Parser::parse_pointer(SharedDataType p_base_type) {
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

UniqueDeclaration Parser::parse_function(SharedDataType p_return_type, const std::string &p_identifier) {
  std::shared_ptr<FunctionType> function_type = std::make_shared<FunctionType>();
  function_type->m_return_type = p_return_type;
  std::vector<UniqueDeclaration> params;

  while (!is_at_end() && !match(Token::Type::CLOSE_PAREN)) {
    DeclarationNode::StorageClassSpec spec;
    SharedDataType data_type = parse_declaration_specifiers(spec);
    UniqueDeclaration param = parse_declarator(data_type);
    param->m_storage_spec = spec;

    function_type->m_param_types.push_back(param->m_data_type);
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
    decl->m_identifier = p_identifier;
    decl->m_params = std::move(params);

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
    decl->m_identifier = p_identifier;
    decl->m_params = std::move(params);
    decl->m_body = std::move(stmt);

    return UniqueDeclaration(decl);
  }

  return nullptr;
}

UniqueDeclaration Parser::parse_array(SharedDataType p_base_type, const std::string &p_identifier) {
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
  decl->m_identifier = p_identifier;

  decl->m_size = std::move(expr);

  return UniqueDeclaration(decl);
}

// Program parsing

UniqueNode Parser::parse_decl_stmt() {
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

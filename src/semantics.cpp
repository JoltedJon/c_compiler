/* Semantic Analysis

Split into multiple stages:
  1) Find Labels for Goto statements
  2) Check for use of undeclared identifiers
  3) Resolve types for expressions
*/

#include <format>
#include <iostream>
#include <memory>

#include "ansi_colors.hpp"
#include "parser.hpp"

namespace JCC {

bool Parser::SemanticContext::has_error = false;

#define fatal_error(p_error_message)                                                                 \
  do {                                                                                               \
    std::cerr << ANSI_COLOR_RED << "Fatal Error - Please Report Bug: " << ANSI_COLOR_RESET << "\n\t" \
              << __PRETTY_FUNCTION__ << ":" << __LINE__ << "\n\t" << std::endl;                      \
    std::abort();                                                                                    \
  } while (0)

void Parser::SemanticContext::error(const Node *node, const std::string &p_error_message) {
  has_error = true;

  if (node == nullptr) return;

  std::cerr << ANSI_COLOR_RED << "Error: " << ANSI_COLOR_RESET
            << std::format("{} {}:{}\n{}", node->m_filename, node->m_line, node->m_column_start, p_error_message)
            << std::endl;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Finding Labels

void Parser::ExpressionNode::find_labels(std::unordered_set<std::string> *labels) { return; }
void Parser::StatementNode::find_labels(std::unordered_set<std::string> *labels) { return; }

void Parser::LabelStmt::find_labels(std::unordered_set<std::string> *labels) {
  auto [it, inserted] = labels->insert(label);
  (void)it;
  if (!inserted) {
    SemanticContext::error(this, std::format(R"(Redefinition of label "{}")", label));
  }
}

void Parser::CaseStmt::find_labels(std::unordered_set<std::string> *labels) { stmt->find_labels(labels); }
void Parser::DefaultStmt::find_labels(std::unordered_set<std::string> *labels) { stmt->find_labels(labels); }

void Parser::CompoundStmt::find_labels(std::unordered_set<std::string> *labels) {
  for (auto &stmt : stmts) {
    stmt->find_labels(labels);
  }
}

void Parser::IfStmt::find_labels(std::unordered_set<std::string> *labels) {
  true_stmt->find_labels(labels);
  if (false_stmt != nullptr) {
    false_stmt->find_labels(labels);
  }
}

void Parser::SwitchStmt::find_labels(std::unordered_set<std::string> *labels) { stmt->find_labels(labels); }
void Parser::WhileStmt::find_labels(std::unordered_set<std::string> *labels) { stmt->find_labels(labels); }
void Parser::ForStmt::find_labels(std::unordered_set<std::string> *labels) { stmt->find_labels(labels); }
void Parser::DeclarationNode::find_labels(std::unordered_set<std::string> *labels) { return; }

void Parser::FunctionDefinition::find_labels(std::unordered_set<std::string> *p_labels) {
  if (body == nullptr) return;
  body->find_labels(&labels);
}

void Parser::TranslationUnit::find_labels(std::unordered_set<std::string> *p_labels) {
  for (auto &decl : program) {
    decl->find_labels(nullptr);
  }
}

/////////////////////////////////////////////////////////////////////////////////////////
// Identifier Resolving

void Parser::UnaryOpNode::resolve_identifiers(SemanticContext *context) { operand->resolve_identifiers(context); }

void Parser::BinaryOpNode::resolve_identifiers(SemanticContext *context) {
  left_operand->resolve_identifiers(context);
  right_operand->resolve_identifiers(context);
}

void Parser::TernaryOpNode::resolve_identifiers(SemanticContext *context) {
  condition->resolve_identifiers(context);
  true_expr->resolve_identifiers(context);
  false_expr->resolve_identifiers(context);
}

void Parser::CallNode::resolve_identifiers(SemanticContext *context) {
  SharedDataType function_type = context->get_identifier_type(name);
  if (function_type == nullptr) {
    context->error(dynamic_cast<Node *>(this), std::format(R"(call to undeclared function "{}")", name));
    return;
  }
  if (function_type->m_kind != DataType::TypeKind::TYPE_FUNCTION) {
    context->error(
        dynamic_cast<Node *>(this),
        std::format(R"(called object type "{}" is not a function or function pointer)", function_type->to_string()));
    return;
  }

  fatal_error("Fix me");
  FunctionType *temp_type = dynamic_cast<FunctionType *>(function_type.get());
  if (args.size() > temp_type->param_types.size()) {
    context->error(dynamic_cast<Node *>(this), std::format(R"(Too many arguments, expected {}, have {})",
                                                           temp_type->param_types.size(), args.size()));
    return;
  }
  if (args.size() < temp_type->param_types.size()) {
    context->error(dynamic_cast<Node *>(this), std::format(R"(Too few arguments, expected {}, have {})",
                                                           temp_type->param_types.size(), args.size()));
    return;
  }

  for (auto &arg : args) {
    arg->resolve_identifiers(context);
  }
}

void Parser::IdentifierNode::resolve_identifiers(SemanticContext *context) {
  SharedDataType identifier_type = context->get_identifier_type(name);
  if (identifier_type == nullptr) {
    context->error(dynamic_cast<Node *>(this), std::format(R"(use of undeclared identifier "{}")", name));
    return;
  }

  d_type = identifier_type;
}

void Parser::ConstantNode::resolve_identifiers(SemanticContext *context) { return; }

void Parser::LabelStmt::resolve_identifiers(SemanticContext *context) { stmt->resolve_identifiers(context); }
void Parser::CaseStmt::resolve_identifiers(SemanticContext *context) { stmt->resolve_identifiers(context); }
void Parser::DefaultStmt::resolve_identifiers(SemanticContext *context) { stmt->resolve_identifiers(context); }

void Parser::CompoundStmt::resolve_identifiers(SemanticContext *context) {
  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = context;

  for (auto &stmt : stmts) {
    stmt->resolve_identifiers(new_context.get());
  }
}

void Parser::ExpressionStmt::resolve_identifiers(SemanticContext *context) {
  if (expr != nullptr) {
    expr->resolve_identifiers(context);
  }
}

void Parser::IfStmt::resolve_identifiers(SemanticContext *context) {
  condition->resolve_identifiers(context);
  true_stmt->resolve_identifiers(context);
  if (false_stmt != nullptr) {
    false_stmt->resolve_identifiers(context);
  }
}

void Parser::SwitchStmt::resolve_identifiers(SemanticContext *context) {
  expr->resolve_identifiers(context);
  stmt->resolve_identifiers(context);
}

void Parser::WhileStmt::resolve_identifiers(SemanticContext *context) {
  condition->resolve_identifiers(context);
  stmt->resolve_identifiers(context);
}

void Parser::ForStmt::resolve_identifiers(SemanticContext *context) {
  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = context;

  // Init clause likely to bring in new identifier so new context is needed
  if (init_clause != nullptr) {
    init_clause->resolve_identifiers(new_context.get());
  }
  if (condition != nullptr) {
    condition->resolve_identifiers(new_context.get());
  }
  if (iteration != nullptr) {
    iteration->resolve_identifiers(new_context.get());
  }

  stmt->resolve_identifiers(new_context.get());
}

void Parser::ControlStmt::resolve_identifiers(SemanticContext *context) { return; }

void Parser::ReturnStmt::resolve_identifiers(SemanticContext *context) {
  if (return_value != nullptr) {
    return_value->resolve_identifiers(context);
  }
}

void Parser::GotoStmt::resolve_identifiers(SemanticContext *context) {
  if (context->labels.find(label) == context->labels.end()) {
    context->error(this, std::format(R"(Use of undeclared label "{}")", label));
  }
}

void Parser::DeclarationNode::resolve_identifiers(SemanticContext *context) {
  context->push_identifier(identifier, m_data_type);
}

void Parser::FunctionDefinition::resolve_identifiers(SemanticContext *context) {
  context->push_identifier(identifier, m_data_type);

  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = context;
  new_context->labels = labels;

  for (auto &param : params) {
    param->resolve_identifiers(new_context.get());
  }
  body->resolve_identifiers(new_context.get());
}

void Parser::TranslationUnit::resolve_identifiers(SemanticContext *context) {
  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = nullptr;

  for (auto &decl : program) {
    decl->resolve_identifiers(new_context.get());
  }
}

/////////////////////////////////////////////////////////////////////////////////////////
// Resolve Types

bool type_compatible(const Parser::DataType *t1, const Parser::DataType *t2) { fatal_error("Not Yet Implemented"); }

Parser::SharedDataType Parser::UnaryOpNode::resolve_types(Parser::SemanticContext *context) {
  SharedDataType operand_type = operand->resolve_types(context);
  if (operand_type == nullptr) return d_type;
  switch (operation) {
    case OpType::OP_POSITIVE:
    case OpType::OP_NEGATIVE:
      if (!operand_type->is_arithemtic()) {
        context->error(this,
                       std::format(R"(Invalid argument type "{}" to unary expression)", operand_type->to_string()));
        return nullptr;
      }
      is_lvalue = false;
      break;
    case OpType::OP_INCREMENT:
    case OpType::OP_DECREMENT:
    case OpType::OP_POST_INCREMENT:
    case OpType::OP_POST_DECREMENT:
      if (!(operand->is_lvalue && operand_type->is_modifiable() && operand_type->is_scalar())) {
        context->error(this, std::format(R"(Invalid argument type "{}" to increment/decrement expression)",
                                         operand_type->to_string()));
        return nullptr;
      }
      is_lvalue = false;
      break;
    case OpType::OP_LOGIC_NOT:
      if (!operand_type->is_scalar()) {
        context->error(this, std::format(R"(Invalid argument type "{}" to "!" expression)", operand_type->to_string()));
        return nullptr;
      }
      d_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
      is_lvalue = false;
      return d_type;
    case OpType::OP_BITWISE_NOT:
      if (!operand_type->is_integer()) {
        context->error(this, std::format(R"(Invalid argument type "{}" to "~" expression)", operand_type->to_string()));
      }
      is_lvalue = false;
    case OpType::OP_ADDRESS_OF:
      if (!operand->is_lvalue) {
        context->error(this, std::format(R"(cannot take address of rvalue type "{}")", operand_type->to_string()));
        return nullptr;
      }
      {
        is_lvalue = false;
        PointerType *temp_type = new PointerType();
        temp_type->m_base_type = operand_type;
        d_type = SharedDataType(temp_type);
        return d_type;
      }
    case OpType::OP_INDIRECTION:
      if (*operand_type == DataType::TypeKind::TYPE_ARRAY) {
        d_type = dynamic_cast<ArrayType *>(operand_type.get())->m_base_type;
        is_lvalue = true;
      }
      else if (*operand_type == DataType::TypeKind::TYPE_FUNCTION) {
        is_lvalue = false;
        PointerType *temp_type = new PointerType();
        temp_type->m_base_type = operand_type;
        d_type = SharedDataType(temp_type);
        return d_type;
      }
      else if (*operand_type == DataType::TypeKind::TYPE_POINTER) {
        is_lvalue = true;
        d_type = dynamic_cast<PointerType *>(operand_type.get())->m_base_type;
        return d_type;
      }
      else {
        context->error(this,
                       std::format(R"(indirection requires pointer type, "{}" is invalid)", operand_type->to_string()));
        return nullptr;
      }
    case OpType::OP_SIZEOF:
      d_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, false, 4);
      is_lvalue = false;
      return d_type;
  }

  d_type = operand_type;

  return d_type;
}

Parser::SharedDataType Parser::BinaryOpNode::resolve_types(Parser::SemanticContext *context) {
  SharedDataType left_type = left_operand->resolve_types(context);
  SharedDataType right_type = right_operand->resolve_types(context);

  if (left_type == nullptr || right_type == nullptr) {
    return nullptr;
  }

  switch (operation) {
    case OpType::OP_SUBTRACTION:
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->m_kind == right_type->m_kind) {
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->is_complete() &&
          right_type->is_integer()) {
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        // TODO do arithmetic conversion
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_ADDITION:
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        // TODO do arithmetic conversion
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      if ((left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->is_complete() &&
           right_type->is_integer())) {
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      if ((right_type->m_kind == DataType::TypeKind::TYPE_POINTER && right_type->is_complete() &&
           left_type->is_integer())) {
        d_type = right_type;
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_MULTIPLICATION:
    case OpType::OP_DIVISION:
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        // TODO do arithmetic conversion
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_MODULO:
      if (left_type->is_integer() && right_type->is_arithemtic()) {
        // TODO do arithmetic conversion
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_BIT_RIGHT:
    case OpType::OP_BIT_LEFT:
      if (left_type->is_integer() && right_type->is_integer()) {
        // TODO do arithmetic conversion
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_BIT_AND:
    case OpType::OP_BIT_OR:
    case OpType::OP_BIT_XOR:
      if (left_type->is_integer() && right_type->is_integer()) {
        // TODO do arithmetic conversion
        d_type = left_type;
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_COMP_GREATER:
    case OpType::OP_COMP_LESS:
    case OpType::OP_COMP_GREATER_EQUAL:
    case OpType::OP_COMP_LESS_EQUAL:
      if ((left_type->is_real() && right_type->is_real()) ||
          (*left_type == DataType::TypeKind::TYPE_POINTER && *right_type == DataType::TypeKind::TYPE_POINTER)) {
        d_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
        is_lvalue = false;
        return d_type;
      }
      break;
    case OpType::OP_COMP_EQUAL:
    case OpType::OP_COMP_NOT_EQUAL:
      // TODO null pointer
      if ((left_type->is_real() && right_type->is_real()) ||
          (*left_type == DataType::TypeKind::TYPE_POINTER && *right_type == DataType::TypeKind::TYPE_POINTER)) {
        d_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
        is_lvalue = false;
        return d_type;
      }
    case OpType::OP_LOGIC_AND:
    case OpType::OP_LOGIC_OR:
      if (left_type->is_scalar() && right_type->is_scalar()) {
        d_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
        is_lvalue = false;
        return d_type;
      }
    case OpType::OP_ASSIGN:
    case OpType::OP_ADD_ASSIGN:
    case OpType::OP_SUBTRACT_ASSIGN:
    case OpType::OP_MULTIPLY_ASSIGN:
    case OpType::OP_DIVIDE_ASSIGN:
    case OpType::OP_MODULO_ASSIGN:
    case OpType::OP_BITWISE_AND_ASSIGN:
    case OpType::OP_BITWISE_OR_ASSIGN:
    case OpType::OP_BITWISE_XOR_ASSIGN:
    case OpType::OP_LEFT_SHIFT_ASSIGN:
    case OpType::OP_RIGHT_SHIFT_ASSIGN:
    case OpType::OP_ARRAY_SUBSCRIPT:
    case OpType::OP_DIRECT_MEM_ACCESS:
    case OpType::OP_INDIRECT_MEM_ACCESS:
      fatal_error("NOT YET IMPLEMENTED");
  }

  SemanticContext::error(
      this, std::format(R"(incompatible types "{}" and "{}")", left_type->to_string(), right_type->to_string()));
  return nullptr;
}

}  // namespace JCC
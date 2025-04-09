/* Semantic Analysis

Split into multiple stages:
  1) Find Labels for Goto statements
  2) Check for use of undeclared identifiers
  3) Resolve types for expressions, Struct/Union validation, Function Call Validation
  4) Constant Folding
  5) Control Flow Check (Return in non-void function, break/continue in loop/switch)
*/

#include "semantics.hpp"

#include <cassert>
#include <cstddef>
#include <format>
#include <iostream>
#include <memory>
#include <unordered_map>

#include "../ansi_colors.hpp"
#include "../ast.hpp"

namespace JCC {

std::unordered_map<std::string, SharedDataType> SemanticContext::base_types;
int SemanticContext::num_errors = 0;

// Debug
std::string stage = "";

#define fatal_error(p_error_message)                                                                                   \
  do {                                                                                                                 \
    std::cerr << ANSI_COLOR_RED << "Fatal Error - Please Report Bug: " << ANSI_COLOR_RESET << "\n\t" << stage          \
              << "\n\t" << __PRETTY_FUNCTION__ << "\n\t" << __FILE__ << ":" << __LINE__ << "\n\t" << (p_error_message) \
              << std::endl;                                                                                            \
    std::abort();                                                                                                      \
  } while (0)

void SemanticContext::error(const Node *node, const std::string &p_error_message) {
  num_errors++;

  if (node == nullptr || num_errors > 10) return;

  std::cerr << ANSI_COLOR_RED << "Error: " << ANSI_COLOR_RESET
            << std::format("{} {}:{}\n{}\n", node->m_filename, node->m_line, node->m_column_start, p_error_message)
            << std::endl;
}

SharedDataType SemanticContext::get_identifier_type(const std::string &p_identifier) {
  SemanticContext *context = this;

  while (context != nullptr) {
    auto it = context->identifiers.find(p_identifier);

    if (it != nullptr) {
      return it->second;
    }

    context = context->previous_context;
  }

  return nullptr;
}

void SemanticContext::push_identifier(const std::string &p_identifier, SharedDataType p_type) {
  identifiers[p_identifier] = std::move(p_type);
}

///////////////////////////////////////
// Data Type Compatibility

bool StructUnionType::compatible(const DataType &p_rhs) const {
  if (p_rhs.m_kind != TypeKind::TYPE_STRUCT && p_rhs.m_kind != TypeKind::TYPE_UNION &&
      p_rhs.m_kind != TypeKind::TYPE_INCOMPLETE)
    return false;

  if (!p_rhs.is_complete() && !is_complete()) return true;

  const StructUnionType rhs = dynamic_cast<const StructUnionType &>(p_rhs);
  if (!m_name.empty() && m_name != rhs.m_name) return false;

  if (m_fields.size() != rhs.m_fields.size()) return false;

  for (size_t i = 0; i < m_fields.size(); ++i) {
    StructUnionField l = m_fields[i];
    StructUnionField r = rhs.m_fields[i];

    if (l.m_name != r.m_name) return false;
    if (!l.m_type->compatible(*r.m_type)) return false;
  }

  return true;
}

bool FunctionType::compatible(const DataType &p_rhs) const {
  if (p_rhs.m_kind != TypeKind::TYPE_FUNCTION) return false;

  const FunctionType rhs = dynamic_cast<const FunctionType &>(p_rhs);

  if (!m_return_type->compatible(*rhs.m_return_type)) return false;

  if (m_param_types.size() != rhs.m_param_types.size()) return false;

  for (size_t i = 0; i < m_param_types.size(); ++i) {
    SharedDataType l = m_param_types[i];
    SharedDataType r = rhs.m_param_types[i];

    if (!l->compatible(*r)) return false;
  }

  return true;
}

bool PointerType::compatible(const DataType &p_rhs) const {
  if (p_rhs.m_kind != TypeKind::TYPE_POINTER) return false;

  const PointerType rhs = dynamic_cast<const PointerType &>(p_rhs);

  if (m_base_type->m_kind == TypeKind::TYPE_VOID || rhs.m_base_type->m_kind == TypeKind::TYPE_VOID) return true;

  return m_base_type->compatible(*rhs.m_base_type);
}

bool ArrayType::compatible(const DataType &p_rhs) const {
  if (p_rhs.m_kind != DataType::TypeKind::TYPE_ARRAY) return false;

  const ArrayType rhs = dynamic_cast<const ArrayType &>(p_rhs);

  if (!m_base_type->compatible(*rhs.m_base_type)) return false;

  if (m_size == 0 || rhs.m_size == 0) return true;

  return m_size == rhs.m_size;
}

///////////////////
// Casting

UniqueExpression implicit_cast(UniqueExpression p_castee, SharedDataType p_cast_type) {
  UnaryOpNode *cast = new UnaryOpNode;
  cast->m_line = p_castee->m_line;
  cast->m_column_start = p_castee->m_column_start;
  cast->m_filename = p_castee->m_filename;
  cast->m_data_type = p_cast_type;

  cast->m_operand = std::move(p_castee);
  cast->m_operation = UnaryOpNode::OpType::OP_CAST;

  return UniqueExpression(cast);
}

SharedDataType arithmetic_conversion(UniqueExpression &lhs, UniqueExpression &rhs) {
  if (*lhs->m_data_type == *rhs->m_data_type) return lhs->m_data_type;

  // TODO Refactor this once operator< is tested

  if (*lhs->m_data_type == *SemanticContext::base_types["double"]) {
    if (!(*lhs->m_data_type < *rhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");
    rhs = implicit_cast(std::move(rhs), SemanticContext::base_types["double"]);
    return lhs->m_data_type;
  }
  if (*rhs->m_data_type == *SemanticContext::base_types["double"]) {
    if (!(*rhs->m_data_type < *lhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");

    lhs = implicit_cast(std::move(lhs), SemanticContext::base_types["double"]);
    return lhs->m_data_type;
  }

  if (*lhs->m_data_type == *SemanticContext::base_types["float"]) {
    if (!(*lhs->m_data_type < *rhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");

    rhs = implicit_cast(std::move(rhs), SemanticContext::base_types["float"]);
    return lhs->m_data_type;
  }
  if (*rhs->m_data_type == *SemanticContext::base_types["float"]) {
    if (!(*rhs->m_data_type < *lhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");

    lhs = implicit_cast(std::move(lhs), SemanticContext::base_types["float"]);
    return lhs->m_data_type;
  }

  if (*lhs->m_data_type == *SemanticContext::base_types["unsigned int"]) {
    if (!(*lhs->m_data_type < *rhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");

    rhs = implicit_cast(std::move(lhs), SemanticContext::base_types["unsigned int"]);
    return lhs->m_data_type;
  }
  if (*rhs->m_data_type == *SemanticContext::base_types["unsigned int"]) {
    if (!(*rhs->m_data_type < *lhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");

    lhs = implicit_cast(std::move(lhs), SemanticContext::base_types["unsigned int"]);
    return lhs->m_data_type;
  }

  if (*lhs->m_data_type == *SemanticContext::base_types["signed int"]) {
    if (!(*lhs->m_data_type < *rhs->m_data_type)) {
      std::cerr << lhs->m_data_type->to_string() << " <-> " << rhs->m_data_type->to_string() << std::endl;
      fatal_error("DataType < operator not implemented correctly");
    }

    rhs = implicit_cast(std::move(lhs), SemanticContext::base_types["signed int"]);
    return lhs->m_data_type;
  }
  if (*rhs->m_data_type == *SemanticContext::base_types["signed int"]) {
    if (!(*rhs->m_data_type < *lhs->m_data_type)) fatal_error("DataType < operator not implemented correctly");

    lhs = implicit_cast(std::move(lhs), SemanticContext::base_types["signed int"]);
    return lhs->m_data_type;
  }

  fatal_error("Not Yet Implemented");
  return lhs->m_data_type;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Finding Labels

void ExpressionNode::find_labels(std::unordered_set<std::string> *labels) { return; }
void StatementNode::find_labels(std::unordered_set<std::string> *labels) { return; }

void LabelStmt::find_labels(std::unordered_set<std::string> *labels) {
  auto [it, inserted] = labels->insert(m_label);
  (void)it;
  if (!inserted) {
    SemanticContext::error(this, std::format(R"(Redefinition of label "{}")", m_label));
  }
}

void CompoundStmt::find_labels(std::unordered_set<std::string> *labels) {
  for (auto &stmt : m_stmts) {
    stmt->find_labels(labels);
  }
}

void IfStmt::find_labels(std::unordered_set<std::string> *labels) {
  m_true_stmt->find_labels(labels);
  if (m_false_stmt != nullptr) {
    m_false_stmt->find_labels(labels);
  }
}

void SwitchStmt::find_labels(std::unordered_set<std::string> *labels) { m_stmt->find_labels(labels); }
void WhileStmt::find_labels(std::unordered_set<std::string> *labels) { m_stmt->find_labels(labels); }
void ForStmt::find_labels(std::unordered_set<std::string> *labels) { m_stmt->find_labels(labels); }
void DeclarationNode::find_labels(std::unordered_set<std::string> *labels) { return; }

void FunctionDefinition::find_labels(std::unordered_set<std::string> *p_labels) {
  if (m_body == nullptr) return;
  m_body->find_labels(&labels);
}

void TranslationUnit::find_labels(std::unordered_set<std::string> *p_labels) {
  stage = "finding labels";
  for (auto &decl : m_program) {
    decl->find_labels(nullptr);
  }
}

/////////////////////////////////////////////////////////////////////////////////////////
// Identifier Resolving

void ExpressionNode::resolve_identifiers(SemanticContext *context) { return; };

void UnaryOpNode::resolve_identifiers(SemanticContext *context) { m_operand->resolve_identifiers(context); }

void BinaryOpNode::resolve_identifiers(SemanticContext *context) {
  m_left_operand->resolve_identifiers(context);
  m_right_operand->resolve_identifiers(context);
}

void TernaryOpNode::resolve_identifiers(SemanticContext *context) {
  m_condition->resolve_identifiers(context);
  m_true_expr->resolve_identifiers(context);
  m_false_expr->resolve_identifiers(context);
}

void MemberAccessNode::resolve_identifiers(SemanticContext *context) { m_expr->resolve_identifiers(context); }

void CallNode::resolve_identifiers(SemanticContext *context) {
  m_callee->resolve_identifiers(context);

  for (auto &arg : m_args) {
    arg->resolve_identifiers(context);
  }
}

void IdentifierNode::resolve_identifiers(SemanticContext *context) {
  SharedDataType identifier_type = context->get_identifier_type(m_name);
  if (identifier_type == nullptr) {
    context->error(dynamic_cast<Node *>(this), std::format(R"(use of undeclared identifier "{}")", m_name));
    return;
  }

  m_data_type = identifier_type;
}

void StatementNode::resolve_identifiers(SemanticContext *context) { return; }

void CompoundStmt::resolve_identifiers(SemanticContext *context) {
  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = context;

  for (auto &stmt : m_stmts) {
    stmt->resolve_identifiers(new_context.get());
  }
}

void ExpressionStmt::resolve_identifiers(SemanticContext *context) {
  if (m_expr != nullptr) {
    m_expr->resolve_identifiers(context);
  }
}

void IfStmt::resolve_identifiers(SemanticContext *context) {
  m_condition->resolve_identifiers(context);
  m_true_stmt->resolve_identifiers(context);
  if (m_false_stmt != nullptr) {
    m_false_stmt->resolve_identifiers(context);
  }
}

void SwitchStmt::resolve_identifiers(SemanticContext *context) {
  m_expr->resolve_identifiers(context);
  m_stmt->resolve_identifiers(context);
}

void WhileStmt::resolve_identifiers(SemanticContext *context) {
  m_condition->resolve_identifiers(context);
  m_stmt->resolve_identifiers(context);
}

void ForStmt::resolve_identifiers(SemanticContext *context) {
  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = context;

  // Init clause likely to bring in new identifier so new context is needed
  if (m_init_clause != nullptr) {
    m_init_clause->resolve_identifiers(new_context.get());
  }
  if (m_condition != nullptr) {
    m_condition->resolve_identifiers(new_context.get());
  }
  if (m_iteration != nullptr) {
    m_iteration->resolve_identifiers(new_context.get());
  }

  m_stmt->resolve_identifiers(new_context.get());
}

void ReturnStmt::resolve_identifiers(SemanticContext *context) {
  if (m_return_value != nullptr) {
    m_return_value->resolve_identifiers(context);
  }
}

void GotoStmt::resolve_identifiers(SemanticContext *context) {
  if (context->labels.find(m_label) == context->labels.end()) {
    context->error(this, std::format(R"(Use of undeclared label "{}")", m_label));
  }
}

void VariableDeclaration::resolve_identifiers(SemanticContext *context) {
  context->push_identifier(m_identifier, m_data_type);
  if (m_initializer != nullptr) {
    m_initializer->resolve_identifiers(context);
  }
}

void FunctionDefinition::resolve_identifiers(SemanticContext *context) {
  context->push_identifier(m_identifier, m_data_type);

  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = context;
  new_context->labels = labels;

  for (auto &param : m_params) {
    param->resolve_identifiers(new_context.get());
  }

  if (m_body != nullptr) {
    m_body->resolve_identifiers(new_context.get());
  }
}

void ArrayDeclaration::resolve_identifiers(SemanticContext *context) {
  context->push_identifier(m_identifier, m_data_type);
  if (m_initializer != nullptr) {
    m_initializer->resolve_identifiers(context);
  }
  if (m_size != nullptr) {
    m_size->resolve_identifiers(context);
  }
}

void TranslationUnit::resolve_identifiers(SemanticContext *context) {
  stage = "resolving identifiers";
  std::unique_ptr<SemanticContext> new_context = std::make_unique<SemanticContext>();
  new_context->previous_context = nullptr;

  for (auto &decl : m_program) {
    decl->resolve_identifiers(new_context.get());
  }
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
// Resolve Types

SharedDataType UnaryOpNode::resolve_types() {
  SharedDataType operand_type = m_operand->resolve_types();

  if (operand_type == nullptr) return nullptr;

  switch (m_operation) {
    case OpType::OP_POSITIVE:
    case OpType::OP_NEGATIVE:
      if (!operand_type->is_arithemtic()) {
        SemanticContext::error(
            this, std::format(R"(Invalid argument type "{}" to unary expression)", operand_type->to_string()));
        return nullptr;
      }
      if (operand_type->m_size < 4) {
        m_operand = implicit_cast(std::move(m_operand), SemanticContext::base_types["signed int"]);
      }
      m_data_type = m_operand->m_data_type;
      m_is_lvalue = false;
      break;
    case OpType::OP_INCREMENT:
    case OpType::OP_DECREMENT:
    case OpType::OP_POST_INCREMENT:
    case OpType::OP_POST_DECREMENT:
      if (!(m_operand->m_is_lvalue && operand_type->is_modifiable())) {
        SemanticContext::error(this,
                               std::format(R"(expression is not assignable)", operand_type->to_string(), to_string()));
        return nullptr;
      }
      if (!operand_type->is_scalar()) {
        SemanticContext::error(this, std::format(R"(Invalid argument type "{}" to "{}" expression)",
                                                 operand_type->to_string(), to_string()));
        return nullptr;
      }
      m_data_type = operand_type;
      m_is_lvalue = false;
      break;
    case OpType::OP_LOGIC_NOT:
      if (!operand_type->is_scalar()) {
        SemanticContext::error(
            this, std::format(R"(Invalid argument type "{}" to "!" expression)", operand_type->to_string()));
        return nullptr;
      }
      m_data_type = SemanticContext::base_types["signed int"];
      m_is_lvalue = false;
      break;
    case OpType::OP_BITWISE_NOT:
      if (!operand_type->is_integer()) {
        SemanticContext::error(
            this, std::format(R"(Invalid argument type "{}" to "~" expression)", operand_type->to_string()));
      }
      if (operand_type->m_size < 4) {
        m_operand = implicit_cast(std::move(m_operand), SemanticContext::base_types["signed int"]);
      }
      m_data_type = m_operand->m_data_type;
      m_is_lvalue = false;
      break;
    case OpType::OP_ADDRESS_OF:
      if (!m_operand->m_is_lvalue) {
        SemanticContext::error(this,
                               std::format(R"(cannot take address of rvalue type "{}")", operand_type->to_string()));
        return nullptr;
      }
      {
        m_is_lvalue = false;
        PointerType *temp_type = new PointerType();
        temp_type->m_base_type = operand_type;
        m_data_type = SharedDataType(temp_type);
        return m_data_type;
      }
    case OpType::OP_INDIRECTION:
      if (*operand_type == DataType::TypeKind::TYPE_ARRAY) {
        m_data_type = dynamic_cast<ArrayType *>(operand_type.get())->m_base_type;
        m_is_lvalue = true;
      }
      else if (*operand_type == DataType::TypeKind::TYPE_FUNCTION) {
        m_is_lvalue = false;
        PointerType *temp_type = new PointerType();
        temp_type->m_base_type = operand_type;
        m_data_type = SharedDataType(temp_type);
        return m_data_type;
      }
      else if (*operand_type == DataType::TypeKind::TYPE_POINTER) {
        m_is_lvalue = true;
        m_data_type = dynamic_cast<PointerType *>(operand_type.get())->m_base_type;
        return m_data_type;
      }
      else {
        SemanticContext::error(
            this, std::format(R"(indirection requires pointer type, "{}" is invalid)", operand_type->to_string()));
        return nullptr;
      }
    case OpType::OP_SIZEOF:
      m_data_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, false, 4);
      m_is_lvalue = false;
      break;
    case OpType::OP_CAST:
      if (m_data_type->m_kind != DataType::TypeKind::TYPE_VOID && !operand_type->is_scalar()) {
        SemanticContext::error(this, std::format(R"(Operand of type "{}" where arithmetic or pointer type is required)",
                                                 operand_type->to_string()));
        return nullptr;
      }
      m_is_lvalue = false;
      break;
  }

  return m_data_type;
}

SharedDataType BinaryOpNode::resolve_types() {
  SharedDataType left_type = m_left_operand->resolve_types();
  SharedDataType right_type = m_right_operand->resolve_types();

  if (left_type == nullptr || right_type == nullptr) {
    return nullptr;
  }

  m_is_lvalue = false;

  switch (m_operation) {
    case OpType::OP_ADDITION:
      // Val + Val
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      // Pointer + Index
      if ((left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->is_complete() &&
           right_type->is_integer())) {
        m_data_type = left_type;
        return m_data_type;
      }
      // Index + Pointer
      if ((right_type->m_kind == DataType::TypeKind::TYPE_POINTER && right_type->is_complete() &&
           left_type->is_integer())) {
        m_data_type = right_type;
        return m_data_type;
      }
      break;
    case OpType::OP_SUBTRACTION:
      // Pointer - Pointer
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->m_kind == right_type->m_kind &&
          left_type->compatible(*right_type)) {
        m_data_type = left_type;
        return m_data_type;
      }
      // Pointer - Index
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->is_complete() &&
          right_type->is_integer()) {
        m_data_type = left_type;
        return m_data_type;
      }
      // Val - Val
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      break;
    case OpType::OP_MULTIPLICATION:
    case OpType::OP_DIVISION:
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      break;
    case OpType::OP_MODULO:
      if (left_type->is_integer() && right_type->is_arithemtic()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      break;
    case OpType::OP_BIT_RIGHT:
    case OpType::OP_BIT_LEFT:
      if (left_type->is_integer() && right_type->is_integer()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      break;
    case OpType::OP_BIT_AND:
    case OpType::OP_BIT_OR:
    case OpType::OP_BIT_XOR:
      if (left_type->is_integer() && right_type->is_integer()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      break;
    case OpType::OP_COMP_GREATER:
    case OpType::OP_COMP_LESS:
    case OpType::OP_COMP_GREATER_EQUAL:
    case OpType::OP_COMP_LESS_EQUAL:
      if ((left_type->is_real() && right_type->is_real()) ||
          (*left_type == DataType::TypeKind::TYPE_POINTER && *right_type == DataType::TypeKind::TYPE_POINTER)) {
        m_data_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
        return m_data_type;
      }
      break;
    case OpType::OP_COMP_EQUAL:
    case OpType::OP_COMP_NOT_EQUAL:
      if ((left_type->is_real() && right_type->is_real()) ||
          (*left_type == DataType::TypeKind::TYPE_POINTER && *right_type == DataType::TypeKind::TYPE_POINTER)) {
        m_data_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
        return m_data_type;
      }
      break;
    case OpType::OP_LOGIC_AND:
    case OpType::OP_LOGIC_OR:
      if (left_type->is_scalar() && right_type->is_scalar()) {
        m_data_type = std::make_shared<DataType>(DataType::TypeKind::TYPE_INT, true, 4);
        return m_data_type;
      }
      break;
    case OpType::OP_ASSIGN:
      m_data_type = left_type;
      if (!(left_type->is_modifiable() && m_left_operand->m_is_lvalue && left_type->is_complete())) {
        SemanticContext::error(this, std::format(R"(Cannot assign to variable "{}" with type "{}")",
                                                 m_left_operand->to_string(), left_type->to_string()));
        return nullptr;
      }
      if ((left_type->is_arithemtic() && right_type->is_arithemtic()) || left_type->compatible(*right_type)) {
        m_right_operand = implicit_cast(std::move(m_right_operand), left_type);
        return m_data_type;
      }
      break;
    case OpType::OP_ADD_ASSIGN:
      m_data_type = left_type;
      // Val + Val
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        m_right_operand = implicit_cast(std::move(m_right_operand), left_type);
        return m_data_type;
      }
      // Pointer + Index
      if ((left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->is_complete() &&
           right_type->is_integer())) {
        return m_data_type;
      }
      break;
    case OpType::OP_SUBTRACT_ASSIGN:
      m_data_type = left_type;
      // Pointer - Pointer
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->m_kind == right_type->m_kind &&
          left_type->compatible(*right_type)) {
        return m_data_type;
      }
      // Pointer - Index
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER && left_type->is_complete() &&
          right_type->is_integer()) {
        return m_data_type;
      }
      // Val - Val
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        m_data_type = arithmetic_conversion(m_left_operand, m_right_operand);
        return m_data_type;
      }
      break;
    case OpType::OP_MULTIPLY_ASSIGN:
    case OpType::OP_DIVIDE_ASSIGN:
    case OpType::OP_MODULO_ASSIGN:
    case OpType::OP_BITWISE_AND_ASSIGN:
    case OpType::OP_BITWISE_OR_ASSIGN:
    case OpType::OP_BITWISE_XOR_ASSIGN:
    case OpType::OP_LEFT_SHIFT_ASSIGN:
    case OpType::OP_RIGHT_SHIFT_ASSIGN:
      m_data_type = left_type;
      if (left_type->is_arithemtic() && right_type->is_arithemtic()) {
        m_right_operand = implicit_cast(std::move(m_right_operand), left_type);
        return m_data_type;
      }
    case OpType::OP_ARRAY_SUBSCRIPT: {
      m_is_lvalue = true;

      PointerType *pointer_expr = nullptr;
      DataType *integer_expr = nullptr;
      if (left_type->m_kind == DataType::TypeKind::TYPE_POINTER) {
        pointer_expr = dynamic_cast<PointerType *>(left_type.get());
      }
      else if (right_type->m_kind == DataType::TypeKind::TYPE_POINTER) {
        pointer_expr = dynamic_cast<PointerType *>(right_type.get());
      }

      if (left_type->is_integer()) {
        integer_expr = left_type.get();
      }
      else if (right_type->is_integer()) {
        integer_expr = right_type.get();
      }

      if (pointer_expr == nullptr || integer_expr == nullptr) {
        break;
      }

      m_data_type = pointer_expr->m_base_type;
      return m_data_type;
    }
    case OpType::OP_COMMA:
      m_data_type = right_type;
      return m_data_type;
  }

  SemanticContext::error(
      this, std::format(R"(incompatible types "{}" and "{}")", left_type->to_string(), right_type->to_string()));
  return nullptr;
}

SharedDataType TernaryOpNode::resolve_types() {
  SharedDataType cond_type = m_condition->resolve_types();
  SharedDataType true_type = m_true_expr->resolve_types();
  SharedDataType false_type = m_false_expr->resolve_types();

  if (!cond_type->is_scalar()) {
    // Used type 'struct s' where arithmetic or pointer type is required
    SemanticContext::error(
        this, std::format(R"(Used type "{}" where arithmetic or pointer type is required)", cond_type->to_string()));
    return nullptr;
  }

  if (!true_type->compatible(*false_type)) {
    // Incompatible operand types ('int' and 'struct s')
    SemanticContext::error(this, std::format(R"~(Incompatible operand types ("{}" and "{}"))~", true_type->to_string(),
                                             false_type->to_string()));
    return nullptr;
  }

  if (true_type->is_arithemtic() && false_type->is_arithemtic()) {
    arithmetic_conversion(m_true_expr, m_false_expr);
    m_data_type = m_true_expr->m_data_type;
    return m_true_expr->m_data_type;
  }

  if (true_type->m_kind == DataType::TypeKind::TYPE_STRUCT || true_type->m_kind == DataType::TypeKind::TYPE_UNION ||
      true_type->m_kind == DataType::TypeKind::TYPE_VOID) {
    m_data_type = true_type;
    return m_data_type;
  }

  if (true_type->m_kind != DataType::TypeKind::TYPE_POINTER && true_type->m_kind != DataType::TypeKind::TYPE_POINTER) {
    m_data_type = true_type;
    return m_data_type;
  }

  PointerType *true_ptr = dynamic_cast<PointerType *>(true_type.get());
  PointerType *false_ptr = dynamic_cast<PointerType *>(false_type.get());

  if (true_ptr->m_base_type->m_kind == DataType::TypeKind::TYPE_VOID &&
      true_ptr->m_base_type->m_kind == DataType::TypeKind::TYPE_VOID) {
    m_data_type = true_type;
    return m_data_type;
  }

  if (true_ptr->m_base_type->m_kind == DataType::TypeKind::TYPE_VOID) {
    m_false_expr = implicit_cast(std::move(m_false_expr), true_type);
    m_data_type = true_type;
    return m_data_type;
  }

  if (false_ptr->m_base_type->m_kind == DataType::TypeKind::TYPE_VOID) {
    m_true_expr = implicit_cast(std::move(m_true_expr), false_type);
    m_data_type = false_type;
    return m_data_type;
  }

  m_data_type = true_type;
  return m_data_type;
}

SharedDataType MemberAccessNode::resolve_types() {
  SharedDataType expr_type = m_expr->resolve_types();

  if (expr_type == nullptr) return nullptr;

  StructUnionType *base_type = nullptr;

  switch (m_access_type) {
    case OpType::OP_INDIRECT_MEM_ACCESS:
      if (*expr_type != DataType::TypeKind::TYPE_POINTER) {
        SemanticContext::error(this,
                               std::format(R"(Member reference type "{}" is not a pointer)", expr_type->to_string()));
        return nullptr;
      }
      base_type = dynamic_cast<StructUnionType *>(dynamic_cast<PointerType *>(expr_type.get())->m_base_type.get());
      break;
    case OpType::OP_DIRECT_MEM_ACCESS:
      // TODO This supposedly will return nullptr on a bad cast
      base_type = dynamic_cast<StructUnionType *>(expr_type.get());
  }

  if (base_type && (*expr_type != DataType::TypeKind::TYPE_STRUCT || *expr_type != DataType::TypeKind::TYPE_UNION)) {
    SemanticContext::error(
        this, std::format(R"(Member reference base type "{}" is not a structure or union)", expr_type->to_string()));
    return nullptr;
  }

  for (auto field : base_type->m_fields) {
    if (m_member == field.m_name) {
      m_data_type = field.m_type;
      m_is_lvalue = m_expr->m_is_lvalue;
      return m_data_type;
    }
  }

  SemanticContext::error(this, std::format(R"(No member named "{}" in "{}")", m_member, base_type->to_string()));
  return nullptr;
}

SharedDataType CallNode::resolve_types() {
  SharedDataType callee_type = m_callee->resolve_types();

  if (callee_type == nullptr) return nullptr;

  if (callee_type->m_kind != DataType::TypeKind::TYPE_FUNCTION) {
    SemanticContext::error(this, std::format(R"(called object type "{}" is not a function or function pointer)",
                                             callee_type->to_string()));
    return nullptr;
  }

  FunctionType *function_type = dynamic_cast<FunctionType *>(callee_type.get());
  if (m_args.size() > function_type->m_param_types.size()) {
    SemanticContext::error(this, std::format(R"(Too many arguments, expected {}, have {})",
                                             function_type->m_param_types.size(), m_args.size()));
    return nullptr;
  }
  if (m_args.size() < function_type->m_param_types.size()) {
    SemanticContext::error(this, std::format(R"(Too few arguments, expected {}, have {})",
                                             function_type->m_param_types.size(), m_args.size()));
    return nullptr;
  }

  for (size_t i = 0; i < m_args.size(); ++i) {
    SharedDataType arg_type = m_args[i]->resolve_types();
    SharedDataType param_type = function_type->m_param_types[i];

    if (!arg_type->compatible(*param_type)) {
      SemanticContext::error(this, std::format(R"(Passing "{}" to parameter of incompatible type "{}")",
                                               arg_type->to_string(), param_type->to_string()));
      return nullptr;
    }

    m_args[i] = implicit_cast(std::move(m_args[i]), param_type);
  }

  m_data_type = function_type->m_return_type;
  m_is_lvalue = false;

  return m_data_type;
}

SharedDataType IdentifierNode::resolve_types() {
  if (m_data_type->m_kind == DataType::TypeKind::TYPE_ARRAY) {
    // Identifier of Array type decays to pointer type when used
    PointerType *p_type = new PointerType;

    p_type->m_base_type = dynamic_cast<ArrayType *>(m_data_type.get())->m_base_type;
    return SharedDataType(p_type);
  }

  m_is_lvalue = true;
  return m_data_type;
}
SharedDataType ConstantNode::resolve_types() {
  m_is_lvalue = m_val_type == ValType::STRING;
  return m_data_type;
}

SharedDataType StatementNode::resolve_types() { return nullptr; }

SharedDataType CaseStmt::resolve_types() {
  m_expr->resolve_types();

  return nullptr;
}

SharedDataType CompoundStmt::resolve_types() {
  for (auto &stmt : m_stmts) {
    stmt->resolve_types();
  }

  return nullptr;
}

SharedDataType ExpressionStmt::resolve_types() {
  if (m_expr == nullptr) return nullptr;

  m_expr->resolve_types();

  return nullptr;
}

SharedDataType IfStmt::resolve_types() {
  SharedDataType cond_type = m_condition->resolve_types();

  if (cond_type == nullptr) return nullptr;

  if (!cond_type->is_scalar()) {
    SemanticContext::error(
        this, std::format(R"(Statement requires expression of scalar type ("{}" invalid))", cond_type->to_string()));
    return nullptr;
  }

  m_true_stmt->resolve_types();

  if (m_false_stmt != nullptr) {
    m_false_stmt->resolve_types();
  }

  return nullptr;
}

SharedDataType SwitchStmt::resolve_types() {
  SharedDataType expr_type = m_expr->resolve_types();

  if (expr_type == nullptr) return nullptr;

  if (!expr_type->is_integer()) {
    SemanticContext::error(
        this, std::format(R"(Statement requires expression of integer type ("{}" invalid))", expr_type->to_string()));
    return nullptr;
  }

  m_stmt->resolve_types();

  return nullptr;
}

SharedDataType WhileStmt::resolve_types() {
  SharedDataType cond_type = m_condition->resolve_types();

  if (cond_type == nullptr) return nullptr;

  if (!cond_type->is_scalar()) {
    SemanticContext::error(
        this, std::format(R"(Statement requires expression of scalar type ("{}" invalid))", cond_type->to_string()));
    return nullptr;
  }

  m_stmt->resolve_types();

  return nullptr;
}

SharedDataType ForStmt::resolve_types() {
  if (m_init_clause != nullptr) {
    if (m_init_clause->resolve_types() == nullptr) return nullptr;
  }
  if (m_condition->resolve_types() == nullptr) return nullptr;
  if (m_iteration != nullptr) {
    if (m_iteration->resolve_types() == nullptr) return nullptr;
  }

  m_stmt->resolve_types();

  return nullptr;
}

SharedDataType ReturnStmt::resolve_types() {
  if (m_return_value == nullptr) {
    return nullptr;
  }

  m_return_value->resolve_types();

  return nullptr;
}

SharedDataType VariableDeclaration::resolve_types() {
  if (m_initializer == nullptr) return nullptr;

  SharedDataType init_type = m_initializer->resolve_types();

  if (init_type == nullptr) return nullptr;

  if (!m_data_type->compatible(*init_type)) {
    SemanticContext::error(
        this, std::format(R"(incompatible types "{}" and "{}")", m_data_type->to_string(), init_type->to_string()));
    return nullptr;
  }

  m_initializer = implicit_cast(std::move(m_initializer), m_data_type);

  return nullptr;
}

SharedDataType FunctionDefinition::resolve_types() {
  m_body->resolve_types();

  return nullptr;
}

SharedDataType ArrayDeclaration::resolve_types() {
  SharedDataType init_type = m_initializer->resolve_types();

  if (init_type == nullptr) return nullptr;

  if (!m_data_type->compatible(*init_type)) {
    SemanticContext::error(
        this, std::format(R"(incompatible types "{}" and "{}")", m_data_type->to_string(), init_type->to_string()));
    return nullptr;
  }

  SharedDataType size_type = m_size->resolve_types();

  if (size_type == nullptr) return nullptr;

  if (!size_type->is_integer()) {
    SemanticContext::error(this, std::format(R"(Size of array has non-integer type "{}")", size_type->to_string()));
    return nullptr;
  }

  return nullptr;
}

SharedDataType TranslationUnit::resolve_types() {
  stage = "resolving types";
  for (auto &decl : m_program) {
    decl->resolve_types();
  }

  return nullptr;
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
// Control Flow

void ExpressionNode::control_flow(FlowContext *p_context) { return; }

void StatementNode::control_flow(FlowContext *p_context) { return; }

void CaseStmt::control_flow(FlowContext *p_context) {
  if (p_context->in_switch == 0) {
    SemanticContext::error(this, R"("case" statement not in switch statement)");
  }
}

void DefaultStmt::control_flow(FlowContext *p_context) {
  if (p_context->in_switch == 0) {
    SemanticContext::error(this, R"("default" statement not in switch statement)");
  }
}

void CompoundStmt::control_flow(FlowContext *p_context) {
  for (auto &stmt : m_stmts) {
    stmt->control_flow(p_context);
  }
}

void IfStmt::control_flow(FlowContext *p_context) {
  m_true_stmt->control_flow(p_context);
  if (m_false_stmt != nullptr) {
    m_false_stmt->control_flow(p_context);
  }
}

void SwitchStmt::control_flow(FlowContext *p_context) {
  p_context->in_switch++;
  m_stmt->control_flow(p_context);
  p_context->in_switch--;
}

void WhileStmt::control_flow(FlowContext *p_context) {
  p_context->in_loop++;
  m_stmt->control_flow(p_context);
  p_context->in_loop--;
}

void ForStmt::control_flow(FlowContext *p_context) {
  p_context->in_loop++;
  m_stmt->control_flow(p_context);
  p_context->in_loop--;
}

void ControlStmt::control_flow(FlowContext *p_context) {
  if (m_control == ControlType::CONTINUE) {
    if (p_context->in_loop == 0) {
      SemanticContext::error(this, R"("continue" statement not in loop statement)");
    }
  }
  else {
    if (p_context->in_loop == 0 && p_context->in_switch == 0) {
      SemanticContext::error(this, R"("break" statement not in loop or switch statement)");
    }
  }
}

void ReturnStmt::control_flow(FlowContext *p_context) {
  p_context->has_return = true;

  if (m_return_value == nullptr && p_context->return_type->m_kind != DataType::TypeKind::TYPE_VOID) {
    SemanticContext::error(this, "Non-void function should return a value");
    return;
  }

  SharedDataType ret_type = m_return_value->m_data_type;
  SharedDataType func_type = p_context->return_type;

  if (!ret_type->compatible(*func_type)) {
    // Returning 'struct s' from a function with incompatible result type 'int'
    SemanticContext::error(this, std::format(R"(Returning "{}" from a function with incompatible result type "{}")",
                                             ret_type->to_string(), func_type->to_string()));
    return;
  }

  m_return_value = implicit_cast(std::move(m_return_value), func_type);
}

void DeclarationNode::control_flow(FlowContext *p_context) { return; }

void FunctionDefinition::control_flow(FlowContext *p_context) {
  std::unique_ptr<FlowContext> context = std::make_unique<FlowContext>();

  context->return_type = dynamic_cast<FunctionType *>(m_data_type.get())->m_return_type;

  m_body->control_flow(context.get());

  if (!context->has_return && context->return_type->m_kind != DataType::TypeKind::TYPE_VOID) {
    SemanticContext::error(this, "control reaches end of non-void function with no return");
  }
}

void TranslationUnit::control_flow(FlowContext *p_context) {
  for (auto &p : m_program) {
    p->control_flow(p_context);
  }
}

}  // namespace JCC
/* Abstract Syntax Tree Graph generation */

#include "graph_gen.hpp"

#include <sstream>

#include "../ansi_colors.hpp"
#include "src/ast.hpp"

namespace JCC {

char *id = nullptr;

void graph_gen(std::ostream &out, const Node *node) {
  id = nullptr;
  node->graph_gen(id++, "", out);
  out << "}\n\n";
}

void Node::graph_gen(const void *parent_id, const void *id, const std::string &name, const std::string &connection,
                     const std::string &color, std::ostream &out) const {
  if (parent_id == nullptr) {
    out << "digraph {\n N" << id << "[label=\"" << name << "\";style=filled;color=" << color << "]\n";
  }
  else {
    out << " N" << parent_id << " -> {N" << id << " [label=\"" << name << "\";style=filled;color=" << color
        << "]} [label=\"" << connection << "\"]\n";
  }
}

void DataType::graph_gen(const void *parent_id, const void *m_id, const std::string &connection,
                         const std::string &name, std::ostream &out) const {
  std::stringstream ss;
  if (m_is_const) {
    ss << "const ";
  }
  if (m_is_restricted) {
    ss << "restrict ";
  }
  if (m_is_volatile) {
    ss << "volatile ";
  }
  switch (m_kind) {
    case TypeKind::TYPE_UNRESOLVED:
      ss << "unresolved";
      break;
    case TypeKind::TYPE_INCOMPLETE:
      ss << "incomplete";
      break;
    case TypeKind::TYPE_VOID:
      ss << "void";
      break;
    case TypeKind::TYPE_INT:
      ss << "int";
      break;
    case TypeKind::TYPE_FLOAT:
      ss << "float";
      break;
    case TypeKind::TYPE_STRUCT:
      ss << "struct";
      break;
    case TypeKind::TYPE_ENUM:
      ss << "enum";
      break;
    case TypeKind::TYPE_UNION:
      ss << "union";
      break;
    case TypeKind::TYPE_FUNCTION:
      ss << "function";
      break;
    case TypeKind::TYPE_POINTER:
      ss << "pointer";
      break;
    case TypeKind::TYPE_ARRAY:
      ss << "array";
      break;
  }
  ss << "\\nsize: " << m_size << " bytes\\n" << name;
  out << " N" << parent_id << " -> {N" << m_id << " [label=\"" << ss.str() << "\"]} [label=\"" << connection << "\"]\n";
}

void StructUnionType::graph_gen(const void *p_parent_id, const void *m_id, const std::string &p_connection,
                                const std::string &p_name, std::ostream &out) const {
  DataType::graph_gen(p_parent_id, m_id, p_connection, p_name, out);
  for (size_t i = 0; i < m_fields.size(); ++i) {
    std::string str = "";
    switch (m_fields[i].m_field_type) {
      case StructUnionField::FieldType::STRUCT_FIELD:
        str += "struct_field ";
        break;
      case StructUnionField::FieldType::UNION_FIELD:
        str += "union_field ";
        break;
    }
    str += m_fields[i].m_name;
    m_fields[i].m_type->graph_gen(p_parent_id, m_id, str, "", out);
  }
}

void FunctionType::graph_gen(const void *p_parent_id, const void *m_id, const std::string &p_connection,
                             const std::string &p_name, std::ostream &out) const {
  DataType::graph_gen(p_parent_id, m_id, p_connection, p_name, out);

  char *child_id = id++;
  m_return_type->graph_gen(m_id, child_id, "return type", "", out);
}

void PointerType::graph_gen(const void *p_parent_id, const void *m_id, const std::string &p_connection,
                            const std::string &p_name, std::ostream &out) const {
  DataType::graph_gen(p_parent_id, m_id, p_connection, p_name, out);
  char *child_id = id++;
  m_base_type->graph_gen(m_id, child_id, "to", "", out);
}

void ArrayType::graph_gen(const void *p_parent_id, const void *m_id, const std::string &p_connection,
                          const std::string &p_name, std::ostream &out) const {
  DataType::graph_gen(p_parent_id, m_id, p_connection, p_name, out);
  char *child_id = id++;
  m_base_type->graph_gen(m_id, child_id, "of", "", out);
}

std::string DataType::to_string() const {
  std::stringstream ss;
  ss << (m_is_const ? "const " : "") << (m_is_restricted ? "restrict " : "") << (m_is_volatile ? "volatile " : "");
  switch (m_kind) {
    case TypeKind::TYPE_UNRESOLVED:
      ss << "unresolved";
      break;
    case TypeKind::TYPE_INCOMPLETE:
      ss << "incomplete";
      break;
    case TypeKind::TYPE_VOID:
      ss << "void";
      break;
    case TypeKind::TYPE_INT:
      ss << "int";
      break;
    case TypeKind::TYPE_FLOAT:
      ss << "float";
      break;
    case TypeKind::TYPE_STRUCT:
      ss << "struct";
      break;
    case TypeKind::TYPE_ENUM:
      ss << "enum";
      break;
    case TypeKind::TYPE_UNION:
      ss << "union";
      break;
    case TypeKind::TYPE_FUNCTION:
      ss << "function";
      break;
    case TypeKind::TYPE_POINTER:
      ss << "pointer";
      break;
    case TypeKind::TYPE_ARRAY:
      ss << "array";
      break;
  }
  return ss.str();
}

std::string UnaryOpNode::to_string() const {
  switch (m_operation) {
    case UnaryOpNode::OpType::OP_POSITIVE:
      return "+";
    case UnaryOpNode::OpType::OP_NEGATIVE:
      return "-";
    case UnaryOpNode::OpType::OP_INCREMENT:
      return "++prefix";
    case UnaryOpNode::OpType::OP_DECREMENT:
      return "--prefix";
    case UnaryOpNode::OpType::OP_POST_INCREMENT:
      return "postfix++";
    case UnaryOpNode::OpType::OP_POST_DECREMENT:
      return "postfix--";
    case UnaryOpNode::OpType::OP_LOGIC_NOT:
      return "!";
    case UnaryOpNode::OpType::OP_BITWISE_NOT:
      return "~";
    case UnaryOpNode::OpType::OP_ADDRESS_OF:
      return "&";
    case UnaryOpNode::OpType::OP_INDIRECTION:
      return "*";
    case UnaryOpNode::OpType::OP_SIZEOF:
      return "sizeof";
    case UnaryOpNode::OpType::OP_CAST:
      return "cast";
  }
  return "";
}
std::string BinaryOpNode::to_string() const {
  switch (m_operation) {
    case BinaryOpNode::OpType::OP_ADDITION:
      return "+";
    case BinaryOpNode::OpType::OP_SUBTRACTION:
      return "-";
    case BinaryOpNode::OpType::OP_MULTIPLICATION:
      return "*";
    case BinaryOpNode::OpType::OP_DIVISION:
      return "/";
    case BinaryOpNode::OpType::OP_MODULO:
      return "%";
    case BinaryOpNode::OpType::OP_BIT_RIGHT:
      return ">>";
    case BinaryOpNode::OpType::OP_BIT_LEFT:
      return "<<";
    case BinaryOpNode::OpType::OP_COMP_GREATER:
      return ">";
    case BinaryOpNode::OpType::OP_COMP_LESS:
      return "<";
    case BinaryOpNode::OpType::OP_COMP_GREATER_EQUAL:
      return ">=";
    case BinaryOpNode::OpType::OP_COMP_LESS_EQUAL:
      return "<=";
    case BinaryOpNode::OpType::OP_COMP_EQUAL:
      return "==";
    case BinaryOpNode::OpType::OP_COMP_NOT_EQUAL:
      return "!=";
    case BinaryOpNode::OpType::OP_BIT_AND:
      return "&";
    case BinaryOpNode::OpType::OP_BIT_OR:
      return "|";
    case BinaryOpNode::OpType::OP_BIT_XOR:
      return "^";
    case BinaryOpNode::OpType::OP_LOGIC_AND:
      return "&&";
    case BinaryOpNode::OpType::OP_LOGIC_OR:
      return "||";
    case BinaryOpNode::OpType::OP_ASSIGN:
      return "=";
    case BinaryOpNode::OpType::OP_ARRAY_SUBSCRIPT:
      return "[";
    case BinaryOpNode::OpType::OP_COMMA:
      return ",";
  }
  return "";
}
std::string TernaryOpNode::to_string() const { return "? :"; }
std::string MemberAccessNode::to_string() const {
  switch (m_access_type) {
    case OpType::OP_DIRECT_MEM_ACCESS:
      return ".";
    case OpType::OP_INDIRECT_MEM_ACCESS:
      return "->";
  }
  return "";
}
std::string CallNode::to_string() const { return "function call"; }
std::string IdentifierNode::to_string() const { return m_name; }
std::string ConstantNode::to_string() const {
  std::stringstream ss;
  std::visit([&](auto &&val) -> void { ss << val; }, m_value);
  return ss.str();
}
std::string LabelStmt::to_string() const { return "label"; }
std::string CaseStmt::to_string() const { return "case"; }
std::string DefaultStmt::to_string() const { return "default"; }
std::string CompoundStmt::to_string() const { return "Compound Statement"; }
std::string ExpressionStmt::to_string() const { return "Expression Statement"; }
std::string IfStmt::to_string() const { return "If"; }
std::string SwitchStmt::to_string() const { return "Switch"; }
std::string WhileStmt::to_string() const {
  switch (m_while_type) {
    case WhileType::WHILE:
      return "While";
    case WhileType::DO:
      return "Do";
  }
  return "";
}
std::string ForStmt::to_string() const { return "For"; }
std::string ControlStmt::to_string() const {
  switch (m_control) {
    case ControlType::BREAK:
      return "break";
    case ControlType::CONTINUE:
      return "continue";
  }
  return "";
}
std::string ReturnStmt::to_string() const { return "return"; }
std::string GotoStmt::to_string() const { return "goto"; }

std::string VariableDeclaration::to_string() const { return "Variable Declaration"; }
std::string FunctionDefinition::to_string() const { return "Function Definition"; }
std::string ArrayDeclaration::to_string() const { return "Array Declaration"; }
std::string TranslationUnit::to_string() const { return "Translation Unit"; }

void ExpressionNode::graph_gen_type(const void *parent_id, std::ostream &out) const {
  m_data_type->graph_gen(parent_id, id++, "type", "", out);
}

void UnaryOpNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(m_id, out);

  m_operand->graph_gen(m_id, "expr", out);
}

void BinaryOpNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(m_id, out);

  m_left_operand->graph_gen(m_id, "left", out);
  m_right_operand->graph_gen(m_id, "right", out);
}

void TernaryOpNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(m_id, out);

  m_condition->graph_gen(m_id, "condition", out);
  m_true_expr->graph_gen(m_id, "true", out);
  m_false_expr->graph_gen(m_id, "false", out);
}

void MemberAccessNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(m_id, out);

  m_expr->graph_gen(m_id, "container", out);
  Node::graph_gen(m_id, &m_member, m_member, "member", "lightskyblue", out);
}

void CallNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string() + ":\\n" + m_name, connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(m_id, out);

  m_callee->graph_gen(m_id, "name", out);
  for (size_t i = 0; i < m_args.size(); ++i) {
    m_args[i]->graph_gen(m_id, "arg " + std::to_string(i + 1), out);
  }
}

void IdentifierNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "khaki3", out);
  ExpressionNode::graph_gen_type(m_id, out);
}
void ConstantNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "lawngreen", out);
  ExpressionNode::graph_gen_type(m_id, out);
}

void LabelStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, id++, to_string() + ":\\n" + m_label, connection, "seagreen2", out);
}

void CaseStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);

  if (m_expr != nullptr) {
    m_expr->graph_gen(m_id, "Expr", out);
  }
}

void DefaultStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, id++, to_string(), connection, "seagreen2", out);
}

void CompoundStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);

  for (size_t i = 0; i < m_stmts.size(); ++i) {
    m_stmts[i]->graph_gen(m_id, std::to_string(i), out);
  }
}

void ExpressionStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);
  if (m_expr != nullptr) {
    m_expr->graph_gen(m_id, "expression", out);
  }
}

void IfStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);
  m_condition->graph_gen(m_id, "condition", out);
  m_true_stmt->graph_gen(m_id, "then", out);
  if (m_false_stmt != nullptr) {
    m_false_stmt->graph_gen(m_id, "else", out);
  }
}

void SwitchStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);
  m_expr->graph_gen(m_id, "Expression", out);
  m_stmt->graph_gen(m_id, "statement", out);
}

void WhileStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);

  if (m_while_type == WhileType::WHILE) {
    m_condition->graph_gen(m_id, "condition", out);
    m_stmt->graph_gen(m_id, "statement", out);
  }
  else {
    m_stmt->graph_gen(m_id, "statement", out);
    m_condition->graph_gen(m_id, "condition", out);
  }
}

void ForStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);

  if (m_init_clause != nullptr) {
    m_init_clause->graph_gen(m_id, "Initialization", out);
  }
  if (m_condition != nullptr) {
    m_condition->graph_gen(m_id, "condition", out);
  }
  if (m_iteration != nullptr) {
    m_iteration->graph_gen(m_id, "iteration", out);
  }

  m_stmt->graph_gen(m_id, "statement", out);
}

void ControlStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, id++, to_string(), connection, "seagreen2", out);
}

void ReturnStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "seagreen2", out);

  if (m_return_value != nullptr) {
    m_return_value->graph_gen(m_id, "return value", out);
  }
}

void GotoStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, id++, to_string() + ":\\n" + m_label, connection, "seagreen2", out);
}

void DeclarationNode::graph_gen_type(const void *parent_id, std::ostream &out) const {
  m_data_type->graph_gen(parent_id, id++, "type", "", out);
}

void VariableDeclaration::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  // TODO better graph generation for declaration
  std::string str = to_string() + ":\\n" + m_identifier;
  Node::graph_gen(parent_id, m_id, str, connection, "tomato2", out);
  DeclarationNode::graph_gen_type(m_id, out);

  if (m_initializer != nullptr) {
    m_initializer->graph_gen(m_id, "initial value", out);
  }
}

void FunctionDefinition::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  // TODO better graph generation for declaration
  std::string str = to_string() + ":\\n" + m_identifier;
  Node::graph_gen(parent_id, m_id, str, connection, "tomato2", out);
  DeclarationNode::graph_gen_type(m_id, out);

  for (size_t i = 0; i < m_params.size(); ++i) {
    m_params[i]->graph_gen(m_id, std::string("arg ") + std::to_string(i), out);
  }

  if (m_body != nullptr) {
    m_body->graph_gen(m_id, "body", out);
  }
}

void ArrayDeclaration::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;

  // TODO better graph generation for declaration
  std::string str = to_string() + " " + m_identifier;
  Node::graph_gen(parent_id, m_id, str, connection, "tomato2", out);
  DeclarationNode::graph_gen_type(m_id, out);

  if (m_initializer != nullptr) {
    m_initializer->graph_gen(m_id, "initial value", out);
  }
  if (m_size != nullptr) {
    m_size->graph_gen(m_id, "size", out);
  }
}

void TranslationUnit::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  char *m_id = id++;
  Node::graph_gen(parent_id, m_id, to_string(), connection, "grey", out);

  for (size_t i = 0; i < m_program.size(); ++i) {
    m_program[i]->graph_gen(m_id, std::to_string(i), out);
  }
}

}  // namespace JCC
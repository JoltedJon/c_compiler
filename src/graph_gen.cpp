/* Abstract Syntax Tree Graph generation */

#include <sstream>

#include "ansi_colors.hpp"
#include "parser.hpp"

namespace JCC {

void Parser::graph_gen(std::ostream &out, const Node *node) const {
  out << ANSI_COLOR_YELLOW "Outputting to GraphViz Compatible format" << ANSI_COLOR_RESET << "\n\n";

  node->graph_gen(nullptr, "", out);
  out << "}\n\n";
}

void Parser::Node::graph_gen(const void *parent_id, const void *id, const std::string &name,
                             const std::string &connection, const std::string &color, std::ostream &out) const {
  if (parent_id == nullptr) {
    out << "digraph {\n N" << id << "[label=\"" << name << "\";style=filled;color=" << color << "]\n";
  }
  else {
    out << " N" << parent_id << " -> {N" << id << " [label=\"" << name << "\";style=filled;color=" << color
        << "]} [label=\"" << connection << "\"]\n";
  }
}

void Parser::DataType::graph_gen(const void *parent_id, const void *id, const std::string &name,
                                 const std::string &connection, std::ostream &out) const {
  out << " N" << parent_id << " -> {N" << id << " [label=\"" << name << "\"]} [label=\"" << connection << "\"]\n";
}

void Parser::DataType::graph_gen(const void *parent_id, const std::string &connection, const std::string &name,
                                 std::ostream &out) const {
  std::stringstream ss;
  if (is_const) {
    ss << "const ";
  }
  if (is_restricted) {
    ss << "restrict ";
  }
  if (is_volatile) {
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
  graph_gen(parent_id, this, ss.str(), connection, out);
}

void Parser::StructUnionType::graph_gen(const void *p_parent_id, const std::string &p_connection,
                                        const std::string &p_name, std::ostream &out) const {
  DataType::graph_gen(p_parent_id, p_connection, name, out);
  for (size_t i = 0; i < fields.size(); ++i) {
    std::string str = "";
    switch (fields[i].field_type) {
      case StructUnionField::FieldType::STRUCT_FIELD:
        str += "struct_field ";
        break;
      case StructUnionField::FieldType::UNION_FIELD:
        str += "union_field ";
        break;
    }
    str += fields[i].name;
    fields[i].type->graph_gen(this, str, "", out);
  }
}

void Parser::FunctionType::graph_gen(const void *p_parent_id, const std::string &p_connection,
                                     const std::string &p_name, std::ostream &out) const {
  DataType::graph_gen(p_parent_id, p_connection, "", out);

  return_type->graph_gen(this, "return type", "", out);
}

void Parser::PointerType::graph_gen(const void *p_parent_id, const std::string &p_connection, const std::string &p_name,
                                    std::ostream &out) const {
  DataType::graph_gen(p_parent_id, p_connection, "", out);
  m_base_type->graph_gen(this, "to", "", out);
}

void Parser::ArrayType::graph_gen(const void *p_parent_id, const std::string &p_connection, const std::string &p_name,
                                  std::ostream &out) const {
  DataType::graph_gen(p_parent_id, p_connection, "", out);
  m_base_type->graph_gen(this, "of", "", out);
}

std::string Parser::DataType::to_string() const {
  switch (m_kind) {
    case TypeKind::TYPE_UNRESOLVED:
      return "unresolved";
    case TypeKind::TYPE_INCOMPLETE:
      return "incomplete";
    case TypeKind::TYPE_VOID:
      return "void";
    case TypeKind::TYPE_INT:
      return "int";
    case TypeKind::TYPE_FLOAT:
      return "float";
    case TypeKind::TYPE_STRUCT:
      return "struct";
    case TypeKind::TYPE_ENUM:
      return "enum";
    case TypeKind::TYPE_UNION:
      return "union";
    case TypeKind::TYPE_FUNCTION:
      return "function";
    case TypeKind::TYPE_POINTER:
      return "pointer";
    case TypeKind::TYPE_ARRAY:
      return "array";
  }
  return "";
}

std::string Parser::UnaryOpNode::to_string() const {
  switch (operation) {
    case Parser::UnaryOpNode::OpType::OP_POSITIVE:
      return "+";
    case Parser::UnaryOpNode::OpType::OP_NEGATIVE:
      return "-";
    case Parser::UnaryOpNode::OpType::OP_INCREMENT:
      return "++prefix";
    case Parser::UnaryOpNode::OpType::OP_DECREMENT:
      return "--prefix";
    case Parser::UnaryOpNode::OpType::OP_POST_INCREMENT:
      return "postfix++";
    case Parser::UnaryOpNode::OpType::OP_POST_DECREMENT:
      return "postfix--";
    case Parser::UnaryOpNode::OpType::OP_LOGIC_NOT:
      return "!";
    case Parser::UnaryOpNode::OpType::OP_BITWISE_NOT:
      return "~";
    case Parser::UnaryOpNode::OpType::OP_ADDRESS_OF:
      return "&";
    case Parser::UnaryOpNode::OpType::OP_INDIRECTION:
      return "*";
    case Parser::UnaryOpNode::OpType::OP_SIZEOF:
      return "sizeof";
  }
  return "";
}
std::string Parser::BinaryOpNode::to_string() const {
  switch (operation) {
    case Parser::BinaryOpNode::OpType::OP_ADDITION:
      return "+";
    case Parser::BinaryOpNode::OpType::OP_SUBTRACTION:
      return "-";
    case Parser::BinaryOpNode::OpType::OP_MULTIPLICATION:
      return "*";
    case Parser::BinaryOpNode::OpType::OP_DIVISION:
      return "/";
    case Parser::BinaryOpNode::OpType::OP_MODULO:
      return "%";
    case Parser::BinaryOpNode::OpType::OP_BIT_RIGHT:
      return ">>";
    case Parser::BinaryOpNode::OpType::OP_BIT_LEFT:
      return "<<";
    case Parser::BinaryOpNode::OpType::OP_COMP_GREATER:
      return ">";
    case Parser::BinaryOpNode::OpType::OP_COMP_LESS:
      return "<";
    case Parser::BinaryOpNode::OpType::OP_COMP_GREATER_EQUAL:
      return ">=";
    case Parser::BinaryOpNode::OpType::OP_COMP_LESS_EQUAL:
      return "<=";
    case Parser::BinaryOpNode::OpType::OP_COMP_EQUAL:
      return "==";
    case Parser::BinaryOpNode::OpType::OP_COMP_NOT_EQUAL:
      return "!=";
    case Parser::BinaryOpNode::OpType::OP_BIT_AND:
      return "&";
    case Parser::BinaryOpNode::OpType::OP_BIT_OR:
      return "|";
    case Parser::BinaryOpNode::OpType::OP_BIT_XOR:
      return "^";
    case Parser::BinaryOpNode::OpType::OP_LOGIC_AND:
      return "&&";
    case Parser::BinaryOpNode::OpType::OP_LOGIC_OR:
      return "||";
    case Parser::BinaryOpNode::OpType::OP_ASSIGN:
      return "=";
    case Parser::BinaryOpNode::OpType::OP_ADD_ASSIGN:
      return "+=";
    case Parser::BinaryOpNode::OpType::OP_SUBTRACT_ASSIGN:
      return "-=";
    case Parser::BinaryOpNode::OpType::OP_MULTIPLY_ASSIGN:
      return "*=";
    case Parser::BinaryOpNode::OpType::OP_DIVIDE_ASSIGN:
      return "/=";
    case Parser::BinaryOpNode::OpType::OP_MODULO_ASSIGN:
      return "%/";
    case Parser::BinaryOpNode::OpType::OP_BITWISE_AND_ASSIGN:
      return "&=";
    case Parser::BinaryOpNode::OpType::OP_BITWISE_OR_ASSIGN:
      return "|=";
    case Parser::BinaryOpNode::OpType::OP_BITWISE_XOR_ASSIGN:
      return "^=";
    case Parser::BinaryOpNode::OpType::OP_LEFT_SHIFT_ASSIGN:
      return "<<=";
    case Parser::BinaryOpNode::OpType::OP_RIGHT_SHIFT_ASSIGN:
      return ">>=";
    case Parser::BinaryOpNode::OpType::OP_ARRAY_SUBSCRIPT:
      return "[";
    case Parser::BinaryOpNode::OpType::OP_DIRECT_MEM_ACCESS:
      return ".";
    case Parser::BinaryOpNode::OpType::OP_INDIRECT_MEM_ACCESS:
      return "->";
  }
  return "";
}
std::string Parser::TernaryOpNode::to_string() const { return "? :"; }
std::string Parser::CallNode::to_string() const { return "function call"; }
std::string Parser::IdentifierNode::to_string() const { return name; }
std::string Parser::ConstantNode::to_string() const {
  std::stringstream ss;
  switch (val_type) {
    case ConstantNode::ValType::INTEGER:
      ss << get_val<uint64_t>();
      break;
    case ConstantNode::ValType::FLOAT:
      ss << get_val<double>();
      break;
    case ConstantNode::ValType::STRING:
      ss << get_val<std::string>();
      break;
  }
  return ss.str();
}
std::string Parser::LabelStmt::to_string() const { return "label"; }
std::string Parser::CaseStmt::to_string() const { return "case"; }
std::string Parser::DefaultStmt::to_string() const { return "default"; }
std::string Parser::CompoundStmt::to_string() const { return "Compound Statement"; }
std::string Parser::ExpressionStmt::to_string() const { return "Expression Statement"; }
std::string Parser::IfStmt::to_string() const { return "If"; }
std::string Parser::SwitchStmt::to_string() const { return "Switch"; }
std::string Parser::WhileStmt::to_string() const {
  switch (while_type) {
    case WhileType::WHILE:
      return "While";
    case WhileType::DO:
      return "Do";
  }
  return "";
}
std::string Parser::ForStmt::to_string() const { return "For"; }
std::string Parser::ControlStmt::to_string() const {
  switch (control) {
    case ControlType::BREAK:
      return "break";
    case ControlType::CONTINUE:
      return "continue";
  }
  return "";
}
std::string Parser::ReturnStmt::to_string() const { return "return"; }
std::string Parser::GotoStmt::to_string() const { return "goto"; }

std::string Parser::VariableDeclaration::to_string() const { return "Variable Declaration"; }
std::string Parser::FunctionDefinition::to_string() const { return "Function Definition"; }
std::string Parser::ArrayDeclaration::to_string() const { return "Array Declaration"; }
std::string Parser::TranslationUnit::to_string() const { return "Translation Unit"; }

void Parser::ExpressionNode::graph_gen_type(std::ostream &out) const { d_type->graph_gen(this, "type", "", out); }

void Parser::UnaryOpNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(out);

  operand->graph_gen(this, "expr", out);
}

void Parser::BinaryOpNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(out);

  left_operand->graph_gen(this, "left", out);
  right_operand->graph_gen(this, "right", out);
}

void Parser::TernaryOpNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(out);

  condition->graph_gen(this, "condition", out);
  true_expr->graph_gen(this, "true", out);
  false_expr->graph_gen(this, "false", out);
}

void Parser::CallNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string() + ":\\n" + name, connection, "lightskyblue", out);
  ExpressionNode::graph_gen_type(out);

  callee->graph_gen(this, "name", out);
  for (size_t i = 0; i < args.size(); ++i) {
    args[i]->graph_gen(this, "arg " + std::to_string(i + 1), out);
  }
}

void Parser::IdentifierNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "khaki3", out);
  ExpressionNode::graph_gen_type(out);
}
void Parser::ConstantNode::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "lawngreen", out);
  ExpressionNode::graph_gen_type(out);
}

void Parser::LabelStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string() + ":\\n" + label, connection, "seagreen2", out);

  stmt->graph_gen(this, "statement", out);
}

void Parser::CaseStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);

  if (expr != nullptr) {
    expr->graph_gen(this, "Expr", out);
  }
  stmt->graph_gen(this, "statement", out);
}

void Parser::DefaultStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);
  stmt->graph_gen(this, "statement", out);
}

void Parser::CompoundStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);

  for (size_t i = 0; i < stmts.size(); ++i) {
    stmts[i]->graph_gen(this, std::to_string(i), out);
  }
}

void Parser::ExpressionStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);
  if (expr != nullptr) {
    expr->graph_gen(this, "expression", out);
  }
}

void Parser::IfStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);
  condition->graph_gen(this, "condition", out);
  true_stmt->graph_gen(this, "then", out);
  if (false_stmt != nullptr) {
    false_stmt->graph_gen(this, "else", out);
  }
}

void Parser::SwitchStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);
  expr->graph_gen(this, "Expression", out);
  stmt->graph_gen(this, "statement", out);
}

void Parser::WhileStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);

  if (while_type == WhileType::WHILE) {
    condition->graph_gen(this, "condition", out);
    stmt->graph_gen(this, "statement", out);
  }
  else {
    stmt->graph_gen(this, "statement", out);
    condition->graph_gen(this, "condition", out);
  }
}

void Parser::ForStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);

  if (init_clause != nullptr) {
    init_clause->graph_gen(this, "Initialization", out);
  }
  if (condition != nullptr) {
    condition->graph_gen(this, "condition", out);
  }
  if (iteration != nullptr) {
    iteration->graph_gen(this, "iteration", out);
  }

  stmt->graph_gen(this, "statement", out);
}

void Parser::ControlStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);
}

void Parser::ReturnStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string(), connection, "seagreen2", out);

  if (return_value != nullptr) {
    return_value->graph_gen(this, "return value", out);
  }
}

void Parser::GotoStmt::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  Node::graph_gen(parent_id, this, to_string() + ":\\n" + label, connection, "seagreen2", out);
}

void Parser::DeclarationNode::graph_gen_type(std::ostream &out) const { m_data_type->graph_gen(this, "type", "", out); }

void Parser::VariableDeclaration::graph_gen(const void *parent_id, const std::string &connection,
                                            std::ostream &out) const {
  // TODO better graph generation for declaration
  std::string str = to_string() + ":\\n" + identifier;
  Node::graph_gen(parent_id, this, str, connection, "tomato2", out);
  DeclarationNode::graph_gen_type(out);

  if (initializer != nullptr) {
    initializer->graph_gen(this, "initial value", out);
  }
}

void Parser::FunctionDefinition::graph_gen(const void *parent_id, const std::string &connection,
                                           std::ostream &out) const {
  // TODO better graph generation for declaration
  std::string str = to_string() + ":\\n" + identifier;
  Node::graph_gen(parent_id, this, str, connection, "tomato2", out);
  DeclarationNode::graph_gen_type(out);

  for (size_t i = 0; i < params.size(); ++i) {
    params[i]->graph_gen(this, std::string("arg ") + std::to_string(i), out);
  }

  if (body != nullptr) {
    body->graph_gen(this, "body", out);
  }
}

void Parser::ArrayDeclaration::graph_gen(const void *parent_id, const std::string &connection,
                                         std::ostream &out) const {
  // TODO better graph generation for declaration
  std::string str = to_string() + " " + identifier;
  Node::graph_gen(parent_id, this, str, connection, "tomato2", out);
  DeclarationNode::graph_gen_type(out);

  if (initializer != nullptr) {
    initializer->graph_gen(this, "initial value", out);
  }
  if (size != nullptr) {
    size->graph_gen(this, "size", out);
  }
}

void Parser::TranslationUnit::graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const {
  // TODO better graph generation for declaration
  Node::graph_gen(parent_id, this, to_string(), connection, "grey", out);

  for (size_t i = 0; i < program.size(); ++i) {
    program[i]->graph_gen(this, std::to_string(i), out);
  }
}

}  // namespace JCC
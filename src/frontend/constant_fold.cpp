/*
Constant folding relies heavily on templating therefore having it's own file is needed
to not go insane from recompilation times

*/

#include <iostream>
#include <stdexcept>
#include <type_traits>

#include "../ansi_colors.hpp"
#include "../ast.hpp"
#include "semantics.hpp"

#define fatal_error(p_error_message)                                                                 \
  do {                                                                                               \
    std::cerr << ANSI_COLOR_RED << "Fatal Error - Please Report Bug: " << ANSI_COLOR_RESET << "\n\t" \
              << __PRETTY_FUNCTION__ << ":" << __LINE__ << "\n\t" << std::endl;                      \
    std::abort();                                                                                    \
  } while (0)

namespace JCC {

/////////////////////////////////////////////////////////////////////////////////////////
// Constant Folding

UniqueExpression ExpressionNode::constant_fold() { return nullptr; }

UniqueExpression UnaryOpNode::constant_fold() {
  UniqueExpression operand;

  if (m_operand->m_node_type != Node::NodeType::CONSTANT) {
    operand = m_operand->constant_fold();
  }
  else {
    operand = std::move(m_operand);
  }

  if (m_operation == OpType::OP_POSITIVE) {
    return operand;
  }

  if (m_operation == OpType::OP_SIZEOF) {
    fatal_error("Size of Not Yet Implemented");
  }

  if (m_operand->m_node_type != NodeType::CONSTANT) {
    if (m_operation != OpType::OP_CAST) return nullptr;

    // If casting to same type, can get rid of the cast
    if (*m_operand->m_data_type == *m_data_type) return std::move(m_operand);
    return nullptr;
  }

  if (operand == nullptr) return nullptr;

  ConstantNode *constant = dynamic_cast<ConstantNode *>(operand.get());
  m_operand = std::move(operand);

  // TODO Bad style needs refactoring
  switch (m_operation) {
    case OpType::OP_POSITIVE:
      return operand;
    case OpType::OP_NEGATIVE:
      switch (constant->m_val_type) {
        case ConstantNode::ValType::SIGNED_CHAR:
          constant->update_val(static_cast<int8_t>(-constant->get_val<int8_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_CHAR:
          constant->update_val(static_cast<uint8_t>(-constant->get_val<uint8_t>()));
          break;
        case ConstantNode::ValType::SIGNED_SHORT:
          constant->update_val(static_cast<int16_t>(-constant->get_val<int16_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_SHORT:
          constant->update_val(static_cast<uint16_t>(-constant->get_val<uint16_t>()));
          break;
        case ConstantNode::ValType::SIGNED_INTEGER:
          constant->update_val(static_cast<int32_t>(-constant->get_val<int32_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_INTEGER:
          constant->update_val(static_cast<uint32_t>(-constant->get_val<uint32_t>()));
          break;
        case ConstantNode::ValType::SIGNED_LONG:
          constant->update_val(static_cast<int64_t>(-constant->get_val<int64_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_LONG:
          constant->update_val(static_cast<uint64_t>(-constant->get_val<uint64_t>()));
          break;
        case ConstantNode::ValType::FLOAT:
          constant->update_val(static_cast<float>(-constant->get_val<float>()));
          break;
        case ConstantNode::ValType::DOUBLE:
          constant->update_val(static_cast<double>(-constant->get_val<double>()));
          break;
        default:
          return nullptr;
      }
      break;
    case OpType::OP_LOGIC_NOT:
      switch (constant->m_val_type) {
        case ConstantNode::ValType::SIGNED_CHAR:
          constant->m_value = static_cast<int32_t>(!constant->get_val<int32_t>());
          break;
        case ConstantNode::ValType::UNSIGNED_CHAR:
          constant->m_value = static_cast<int32_t>(!constant->get_val<int32_t>());
          break;
        case ConstantNode::ValType::SIGNED_SHORT:
          constant->m_value = static_cast<int32_t>(!constant->get_val<int32_t>());
          break;
        case ConstantNode::ValType::UNSIGNED_SHORT:
          constant->m_value = static_cast<int32_t>(!constant->get_val<int32_t>());
          break;
        case ConstantNode::ValType::SIGNED_INTEGER:
          constant->m_value = static_cast<int32_t>(!constant->get_val<int32_t>());
          break;
        case ConstantNode::ValType::UNSIGNED_INTEGER:
          constant->m_value = static_cast<int32_t>(!constant->get_val<uint32_t>());
          break;
        case ConstantNode::ValType::SIGNED_LONG:
          constant->m_value = static_cast<int32_t>(!constant->get_val<int64_t>());
          break;
        case ConstantNode::ValType::UNSIGNED_LONG:
          constant->m_value = static_cast<int32_t>(!constant->get_val<uint64_t>());
          break;
        case ConstantNode::ValType::FLOAT:
          constant->m_value = static_cast<int32_t>(!constant->get_val<float>());
          break;
        case ConstantNode::ValType::DOUBLE:
          constant->m_value = static_cast<int32_t>(!constant->get_val<float>());
          break;
        default:
          return nullptr;
      }
      constant->m_val_type = ConstantNode::ValType::SIGNED_INTEGER;
      break;
    case OpType::OP_BITWISE_NOT:
      switch (constant->m_val_type) {
        case ConstantNode::ValType::SIGNED_CHAR:
          constant->update_val(static_cast<int8_t>(~constant->get_val<int8_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_CHAR:
          constant->update_val(static_cast<uint8_t>(~constant->get_val<uint8_t>()));
          break;
        case ConstantNode::ValType::SIGNED_SHORT:
          constant->update_val(static_cast<int16_t>(~constant->get_val<int16_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_SHORT:
          constant->update_val(static_cast<uint16_t>(~constant->get_val<uint16_t>()));
          break;
        case ConstantNode::ValType::SIGNED_INTEGER:
          constant->update_val(static_cast<int32_t>(~constant->get_val<int32_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_INTEGER:
          constant->update_val(static_cast<uint32_t>(~constant->get_val<uint32_t>()));
          break;
        case ConstantNode::ValType::SIGNED_LONG:
          constant->update_val(static_cast<int64_t>(~constant->get_val<int64_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_LONG:
          constant->update_val(static_cast<uint64_t>(~constant->get_val<uint64_t>()));
          break;
        default:
          return nullptr;
      }

      break;
    case OpType::OP_CAST:
      // Can't constant fold pointer types
      if (!m_data_type->is_arithemtic()) return nullptr;
      switch (constant->m_val_type) {
        case ConstantNode::ValType::SIGNED_CHAR:
          constant->update_val(m_data_type->cast(constant->get_val<int8_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_CHAR:
          constant->update_val(m_data_type->cast(constant->get_val<uint8_t>()));
          break;
        case ConstantNode::ValType::SIGNED_SHORT:
          constant->update_val(m_data_type->cast(constant->get_val<int16_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_SHORT:
          constant->update_val(m_data_type->cast(constant->get_val<uint16_t>()));
          break;
        case ConstantNode::ValType::SIGNED_INTEGER:
          constant->update_val(m_data_type->cast(constant->get_val<uint32_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_INTEGER:
          constant->update_val(m_data_type->cast(constant->get_val<uint32_t>()));
          break;
        case ConstantNode::ValType::SIGNED_LONG:
          constant->update_val(m_data_type->cast(constant->get_val<int64_t>()));
          break;
        case ConstantNode::ValType::UNSIGNED_LONG:
          constant->update_val(m_data_type->cast(constant->get_val<uint64_t>()));
          break;
        case ConstantNode::ValType::FLOAT:
          constant->update_val(m_data_type->cast(constant->get_val<float>()));
          break;
        case ConstantNode::ValType::DOUBLE:
          constant->update_val(m_data_type->cast(constant->get_val<double>()));
          break;
        default:
          return nullptr;
      }
      break;
    default:
      return nullptr;
  }
  constant->m_data_type = m_data_type->clone();

  return operand;
}

template <typename Op>
LiteralType binary_op(const LiteralType &lhs, const LiteralType &rhs, Op op) {
  return std::visit(
      [&](auto &&lhs, auto &&rhs) -> LiteralType {
        using L = std::decay_t<decltype(rhs)>;
        using R = std::decay_t<decltype(lhs)>;

        if constexpr (std::is_arithmetic_v<L> && std::is_arithmetic_v<R>) {
          using Common = std::common_type_t<L, R>;
          return static_cast<Common>(op(static_cast<Common>(lhs), static_cast<Common>(rhs)));
        }
        else {
          fatal_error("Unsupported operation type");
        }
      },
      lhs, rhs);
}

template <typename Op>
LiteralType binary_integral_op(const LiteralType &lhs, const LiteralType &rhs, Op op) {
  return std::visit(
      [&](auto &&lhs, auto &&rhs) -> LiteralType {
        using L = std::decay_t<decltype(rhs)>;
        using R = std::decay_t<decltype(lhs)>;

        if constexpr (std::is_integral_v<L> && std::is_integral_v<R>) {
          using Common = std::common_type_t<L, R>;
          return static_cast<Common>(op(static_cast<Common>(lhs), static_cast<Common>(rhs)));
        }
        else {
          fatal_error("Unsupported operation type");
        }
      },
      lhs, rhs);
}

struct BitRight {
  template <typename T>
  T operator()(const T &a, const T &b) const {
    return a >> b;
  }
};

struct BitLeft {
  template <typename T>
  T operator()(const T &a, const T &b) const {
    return a << b;
  }
};

UniqueExpression BinaryOpNode::constant_fold() {
  if (m_left_operand->m_node_type != Node::NodeType::CONSTANT) {
    UniqueExpression opt_lhs = m_left_operand->constant_fold();
    if (opt_lhs != nullptr) {
      m_left_operand = std::move(opt_lhs);
    }
  }

  if (m_right_operand->m_node_type != Node::NodeType::CONSTANT) {
    UniqueExpression opt_rhs = m_right_operand->constant_fold();
    if (opt_rhs != nullptr) {
      m_right_operand = std::move(opt_rhs);
    }
  }

  // Can't fold anymore if one is not a constant
  if ((m_right_operand == nullptr || m_right_operand->m_node_type != NodeType::CONSTANT) ||
      (m_left_operand == nullptr || m_left_operand->m_node_type != NodeType::CONSTANT))
    return nullptr;

  ConstantNode *constant = new ConstantNode;
  constant->m_line = m_line;
  constant->m_column_start = m_column_start;
  constant->m_filename = m_filename;
  constant->m_data_type = m_data_type->clone();
  constant->m_is_lvalue = m_is_lvalue;

  UniqueExpression expr(constant);

  ConstantNode *lhs = dynamic_cast<ConstantNode *>(m_left_operand.get());
  ConstantNode *rhs = dynamic_cast<ConstantNode *>(m_right_operand.get());

  switch (m_operation) {
    case OpType::OP_ADDITION:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::plus<>{}));
      break;
    case OpType::OP_SUBTRACTION:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::minus<>{}));
      break;
    case OpType::OP_MULTIPLICATION:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::multiplies<>{}));
      break;
    case OpType::OP_DIVISION:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::divides<>{}));
      break;
    case OpType::OP_MODULO:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::modulus<>{}));
      break;
    case OpType::OP_BIT_AND:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::bit_and<>{}));
      break;
    case OpType::OP_BIT_OR:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::bit_or<>{}));
      break;
    case OpType::OP_BIT_XOR:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::bit_xor<>{}));
      break;
    case OpType::OP_COMP_GREATER:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::greater<>{}));
      break;
    case OpType::OP_COMP_LESS:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::less<>{}));
      break;
    case OpType::OP_COMP_GREATER_EQUAL:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::greater_equal<>{}));
      break;
    case OpType::OP_COMP_LESS_EQUAL:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, std::less_equal<>{}));
      break;
    case OpType::OP_COMP_EQUAL:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::equal_to<>{}));
      break;
    case OpType::OP_COMP_NOT_EQUAL:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::not_equal_to<>{}));
      break;
    case OpType::OP_LOGIC_AND:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::logical_and<>{}));
      break;
    case OpType::OP_LOGIC_OR:
      constant->update_val(binary_op(lhs->m_value, rhs->m_value, std::logical_or<>{}));
      break;
    case OpType::OP_BIT_RIGHT:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, BitRight{}));
      break;
    case OpType::OP_BIT_LEFT:
      constant->update_val(binary_integral_op(lhs->m_value, rhs->m_value, BitLeft{}));
      break;
    default:
      return nullptr;
  }

  return expr;
}

UniqueExpression TernaryOpNode::constant_fold() {
  UniqueExpression condition;
  if (m_condition->m_node_type != NodeType::CONSTANT) {
    condition = m_condition->constant_fold();
  }
  else {
    condition = std::move(m_condition);
  }

  UniqueExpression true_expr;
  if (m_true_expr->m_node_type != NodeType::CONSTANT) {
    true_expr = m_true_expr->constant_fold();
  }
  else {
    true_expr = std::move(m_true_expr);
  }

  UniqueExpression false_expr;
  if (m_false_expr->m_node_type != NodeType::CONSTANT) {
    false_expr = m_false_expr->constant_fold();
  }
  else {
    false_expr = std::move(m_false_expr);
  }

  if (condition != nullptr) {
    m_condition = std::move(condition);
  }
  if (true_expr != nullptr) {
    m_true_expr = std::move(true_expr);
  }
  if (false_expr != nullptr) {
    m_false_expr = std::move(false_expr);
  }

  if (condition == nullptr || condition->m_node_type != NodeType::CONSTANT) {
    return nullptr;
  }

  ConstantNode *constant = dynamic_cast<ConstantNode *>(m_condition.get());

  if (std::visit(
          [&](auto &&val) -> bool {
            if constexpr (std::is_arithmetic_v<std::decay<decltype(val)>>) {
              return static_cast<bool>(val);
            }
            fatal_error("Unsupported type");
          },
          constant->m_value)) {
    return std::move(m_true_expr);
  }
  return std::move(m_false_expr);
}

UniqueExpression MemberAccessNode::constant_fold() {
  UniqueExpression expr = m_expr->constant_fold();

  if (expr != nullptr) {
    m_expr = std::move(expr);
  }

  return nullptr;
}

UniqueExpression CallNode::constant_fold() {
  for (size_t i = 0; i < m_args.size(); ++i) {
    UniqueExpression arg = m_args[i]->constant_fold();

    if (arg != nullptr) {
      m_args[i] = std::move(arg);
    }
  }

  return nullptr;
}

UniqueExpression IdentifierNode::constant_fold() { return nullptr; }

UniqueExpression ConstantNode::constant_fold() { return nullptr; }

UniqueExpression StatementNode::constant_fold() { return nullptr; }

UniqueExpression CaseStmt::constant_fold() {
  UniqueExpression expr = m_expr->constant_fold();

  if (expr != nullptr) {
    m_expr = std::move(expr);
  }

  if (m_expr->m_node_type != NodeType::CONSTANT) {
    SemanticContext::error(m_expr.get(), "Expression is not an integer constant expression");
  }

  return nullptr;
}

UniqueExpression CompoundStmt::constant_fold() {
  for (size_t i = 0; i < m_stmts.size(); ++i) {
    UniqueExpression stmt = m_stmts[i]->constant_fold();

    if (stmt != nullptr) {
      m_stmts[i] = std::move(stmt);
    }
  }

  return nullptr;
}

UniqueExpression ExpressionStmt::constant_fold() {
  UniqueExpression expr = m_expr->constant_fold();

  if (expr != nullptr) {
    m_expr = std::move(expr);
  }

  return nullptr;
}

UniqueExpression IfStmt::constant_fold() {
  UniqueExpression condition = m_condition->constant_fold();

  if (condition != nullptr) {
    m_condition = std::move(condition);
  }

  m_true_stmt->constant_fold();
  m_false_stmt->constant_fold();

  return nullptr;
}

UniqueExpression SwitchStmt::constant_fold() {
  UniqueExpression expr = m_expr->constant_fold();

  if (expr != nullptr) {
    m_expr = std::move(expr);
  }

  m_stmt->constant_fold();

  return nullptr;
}

UniqueExpression WhileStmt::constant_fold() {
  UniqueExpression expr = m_condition->constant_fold();

  if (expr != nullptr) {
    m_condition = std::move(expr);
  }

  m_stmt->constant_fold();

  return nullptr;
}

UniqueExpression ForStmt::constant_fold() {
  if (m_init_clause != nullptr) {
    m_init_clause->constant_fold();
  }

  if (m_condition != nullptr) {
    UniqueExpression condition = m_condition->constant_fold();

    if (condition != nullptr) {
      m_condition = std::move(condition);
    }
  }

  if (m_iteration != nullptr) {
    UniqueExpression iter = m_iteration->constant_fold();

    if (iter != nullptr) {
      m_iteration = std::move(iter);
    }
  }

  m_stmt->constant_fold();

  return nullptr;
}

UniqueExpression ReturnStmt::constant_fold() {
  if (m_return_value == nullptr) return nullptr;

  UniqueExpression ret = m_return_value->constant_fold();

  if (ret != nullptr) {
    m_return_value = std::move(ret);
  }

  return nullptr;
}

UniqueExpression VariableDeclaration::constant_fold() {
  if (m_initializer == nullptr) return nullptr;

  UniqueExpression init = m_initializer->constant_fold();

  if (init != nullptr) {
    m_initializer = std::move(init);
  }

  return nullptr;
}

UniqueExpression FunctionDefinition::constant_fold() {
  if (m_body == nullptr) return nullptr;

  return m_body->constant_fold();
}

UniqueExpression ArrayDeclaration::constant_fold() {
  if (m_initializer != nullptr) {
    UniqueExpression init = m_initializer->constant_fold();

    if (init != nullptr) {
      m_initializer = std::move(init);
    }
  }

  if (m_size != nullptr) {
    UniqueExpression size = m_size->constant_fold();

    if (size != nullptr) {
      m_size = std::move(size);
    }
  }

  return nullptr;
}

UniqueExpression TranslationUnit::constant_fold() {
  for (auto &decl : m_program) {
    decl->constant_fold();
  }

  return nullptr;
}

}  // namespace JCC
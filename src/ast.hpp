#pragma once

#include <llvm-18/llvm/IR/Value.h>

#include <cassert>
#include <cstdint>
#include <memory>
#include <unordered_set>
#include <vector>

#include "frontend/lexer.hpp"

namespace JCC {

struct DataType;

struct Node;
struct ExpressionNode;
struct StatementNode;
struct DeclarationNode;

struct SemanticContext;
struct FlowContext;

using SharedDataType = std::shared_ptr<DataType>;
using UniqueNode = std::unique_ptr<Node>;
using UniqueExpression = std::unique_ptr<ExpressionNode>;
using UniqueStatement = std::unique_ptr<StatementNode>;
using UniqueDeclaration = std::unique_ptr<DeclarationNode>;

//////////////////////////////////////////////////////////////////////////////////
// Data Types

enum class TypeQualifier { NONE, CONST, VOLATILE, RESTRICT };
struct DataType {
  enum class TypeKind {
    TYPE_UNRESOLVED,
    TYPE_INCOMPLETE,  // Used for forward declarations and parsing struct/union
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_UNION,
    TYPE_FUNCTION,
    TYPE_POINTER,
    TYPE_ARRAY
  };
  TypeKind m_kind = TypeKind::TYPE_UNRESOLVED;

  bool m_is_const = false;
  bool m_is_restricted = false;
  bool m_is_volatile = false;

  bool m_is_signed = false;
  unsigned int m_size = 0;  // Size in bytes

  DataType() = default;
  DataType(TypeKind p_kind, bool p_is_signed, unsigned int p_size)
      : m_kind(p_kind), m_is_signed(p_is_signed), m_size(p_size) {}

  inline void set_qualifiers(const std::unordered_set<TypeQualifier> &qualifiers) {
    m_is_const = qualifiers.contains(TypeQualifier::CONST);
    m_is_restricted = qualifiers.contains(TypeQualifier::RESTRICT);
    m_is_volatile = qualifiers.contains(TypeQualifier::VOLATILE);
  }

  inline bool operator==(const DataType &p_rhs) const {
    // Bools for debugging
    bool same_kind = m_kind == p_rhs.m_kind;
    bool same_sign = m_is_signed == p_rhs.m_is_signed;
    bool same_size = m_size == p_rhs.m_size;
    return same_kind && same_sign && same_size;
  }
  inline bool operator!=(const DataType &p_rhs) const { return !(operator==(p_rhs)); }
  inline bool operator==(const TypeKind &other_kind) const { return m_kind == other_kind; }
  inline bool operator!=(const TypeKind &other_kind) const { return !(operator==(other_kind)); }

  inline bool operator<(const DataType &p_rhs) const {
    if (operator==(p_rhs)) return false;
    if (m_kind == p_rhs.m_kind) {
      if (m_size == p_rhs.m_size) {
        return m_is_signed;
      }
      return true;
    }
    return m_kind != TypeKind::TYPE_FLOAT;
  }

  virtual SharedDataType clone() const { return std::make_shared<DataType>(*this); }

  // Semantic Analysis
  inline bool is_character() const { return m_kind == TypeKind::TYPE_INT && m_size == 1; }
  inline bool is_integer() const { return m_kind == TypeKind::TYPE_INT || m_kind == TypeKind::TYPE_ENUM; }
  inline bool is_real() const { return is_integer() || m_kind == TypeKind::TYPE_FLOAT; }
  inline bool is_arithemtic() const { return m_kind == TypeKind::TYPE_INT || m_kind == TypeKind::TYPE_FLOAT; }
  inline bool is_scalar() const {
    return is_arithemtic() || m_kind == DataType::TypeKind::TYPE_ENUM || m_kind == TypeKind::TYPE_POINTER;
  }
  virtual inline bool is_modifiable() const { return !m_is_const && (is_scalar()); }
  virtual inline bool is_complete() const {
    return (m_kind != TypeKind::TYPE_INCOMPLETE && m_kind != TypeKind::TYPE_UNRESOLVED &&
            m_kind != TypeKind::TYPE_VOID);
  }

  virtual inline bool compatible(const DataType &p_rhs) const { return operator==(p_rhs); }

  template <typename T>
  inline LiteralType cast(T castee) {
    if (m_kind == TypeKind::TYPE_FLOAT) {
      if (m_size == 4) {
        return static_cast<float>(castee);
      }
      return static_cast<double>(castee);
    }
    if (m_is_signed) {
      switch (m_size) {
        case 1:
          return static_cast<int8_t>(castee);
        case 2:
          return static_cast<int16_t>(castee);
        case 4:
          return static_cast<int32_t>(castee);
        case 8:
          return static_cast<int64_t>(castee);
        default:
          assert(false);
      }
    }
    switch (m_size) {
      case 1:
        return static_cast<uint8_t>(castee);
      case 2:
        return static_cast<uint16_t>(castee);
      case 4:
        return static_cast<uint32_t>(castee);
      case 8:
        return static_cast<uint64_t>(castee);
      default:
        assert(false);
    }
  }

  // Graph Generation Debugging functions
  std::string to_string() const;
  virtual void graph_gen(const void *parent_id, const void *id, const std::string &connection, const std::string &name,
                         std::ostream &out) const;
};

struct StructUnionType : public DataType {
  struct StructUnionField {
    enum class FieldType { STRUCT_FIELD, UNION_FIELD } m_field_type = FieldType::STRUCT_FIELD;
    std::string m_name;
    SharedDataType m_type;
  };
  std::vector<StructUnionField> m_fields;
  std::string m_name = "";  // if "" then it's anonymous
  unsigned int m_alignment = 0;

  StructUnionType() { m_kind = TypeKind::TYPE_INCOMPLETE; }

  virtual inline bool is_modifiable() const override {
    if (m_is_const) return false;
    for (auto &field : m_fields) {
      if (!field.m_type->is_modifiable()) {
        return false;
      }
    }
    return true;
  }

  virtual bool compatible(const DataType &p_rhs) const override;

  virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
  virtual void graph_gen(const void *parent_id, const void *id, const std::string &connection, const std::string &name,
                         std::ostream &out) const override;
};

struct EnumType : public DataType {
  EnumType() {
    m_kind = TypeKind::TYPE_ENUM;
    m_size = 4;  // Size of Enum is same size as Integer type
  }

  virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
};

struct FunctionType : public DataType {
  SharedDataType m_return_type;
  std::vector<SharedDataType> m_param_types;

  FunctionType() { m_kind = TypeKind::TYPE_FUNCTION; }

  virtual bool compatible(const DataType &p_rhs) const override;

  virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
  virtual void graph_gen(const void *parent_id, const void *id, const std::string &connection, const std::string &name,
                         std::ostream &out) const override;
};

struct PointerType : public DataType {
  SharedDataType m_base_type = nullptr;

  PointerType() { m_kind = TypeKind::TYPE_POINTER; }

  virtual inline bool is_complete() const override {
    return *m_base_type == TypeKind::TYPE_POINTER || m_base_type->is_complete();
  }
  virtual bool compatible(const DataType &p_rhs) const override;

  virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
  virtual void graph_gen(const void *parent_id, const void *id, const std::string &connection, const std::string &name,
                         std::ostream &out) const override;
};

struct ArrayType : public DataType {
  SharedDataType m_base_type = nullptr;

  ArrayType() { m_kind = TypeKind::TYPE_ARRAY; }

  virtual inline bool is_complete() const override { return m_size > 0 && m_base_type->is_complete(); }
  virtual bool compatible(const DataType &p_rhs) const override;

  virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
  virtual void graph_gen(const void *parent_id, const void *id, const std::string &connection, const std::string &name,
                         std::ostream &out) const override;
};

//////////////////////////////////////////////////////////////////////////////////
// Abstract Syntax Tree Nodes

struct Node {
  enum class NodeType {
    NONE,
    UNARY_EXPRESSION,
    BINARY_EXPRESSION,
    TERNARY_EXPRESSION,
    MEMBER_ACCESS_EXPRESSION,
    FUNCTION_CALL,
    IDENTIFIER,
    CONSTANT,
    // Statements
    LABEL_STATEMENT,
    CASE_STATEMENT,
    DEFAULT_STATEMENT,
    COMPOUND_STATEMENT,
    EXPRESSION_STATEMENT,
    IF_STATEMENT,
    SWITCH_STATEMENT,
    WHILE_STATEMENT,
    FOR_STATEMENT,
    CONTROL_STATEMENT,  // break, continue
    RETURN_STATEMENT,
    GOTO_STATEMENT,
    // Declarations
    VARIABLE_DECLARATION,
    ARRAY_DECLARATION,
    FUNCTION_DECLARATION,
    FUNCTION_DEFINITION,
    // Program
    TRANSLATION_UNIT,
  };

  NodeType m_node_type = NodeType::NONE;
  size_t m_line = 0;
  size_t m_column_start = 0;
  std::string m_filename = "";

  virtual ~Node() = default;

  // Semantic Analysis
  virtual void find_labels(std::unordered_set<std::string> *labels) = 0;
  virtual void resolve_identifiers(SemanticContext *context) = 0;
  virtual SharedDataType resolve_types() = 0;
  virtual UniqueExpression constant_fold() = 0;
  virtual void control_flow(FlowContext *context) = 0;

  // Code Generation
  virtual llvm::Value *codegen() = 0;

  // Graph Generation For debugging
  virtual std::string to_string() const = 0;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const = 0;
  void graph_gen(const void *parent_id, const void *child_id, const std::string &name, const std::string &connection,
                 const std::string &color, std::ostream &out) const;
};

//////////////
// Expressions

struct ExpressionNode : public Node {
  SharedDataType m_data_type;
  bool m_is_lvalue = false;

  virtual ~ExpressionNode() = default;

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  void graph_gen_type(const void *parent_id, std::ostream &out) const;

 protected:
  ExpressionNode() = default;
};

struct UnaryOpNode : public ExpressionNode {
  enum class OpType {
    OP_POSITIVE,
    OP_NEGATIVE,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_POST_INCREMENT,
    OP_POST_DECREMENT,
    OP_LOGIC_NOT,
    OP_BITWISE_NOT,
    OP_ADDRESS_OF,
    OP_INDIRECTION,
    OP_SIZEOF,
    OP_CAST
  };

  OpType m_operation = OpType::OP_POSITIVE;
  UniqueExpression m_operand = nullptr;

  UnaryOpNode() { m_node_type = NodeType::UNARY_EXPRESSION; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  // Graph Generation
  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct BinaryOpNode : public ExpressionNode {
  enum class OpType {
    // Math
    OP_ADDITION,
    OP_SUBTRACTION,
    OP_MULTIPLICATION,
    OP_DIVISION,
    OP_MODULO,
    // Bitshift
    OP_BIT_RIGHT,
    OP_BIT_LEFT,
    // Bitwise
    OP_BIT_AND,
    OP_BIT_OR,
    OP_BIT_XOR,
    // Comparison
    OP_COMP_GREATER,
    OP_COMP_LESS,
    OP_COMP_GREATER_EQUAL,
    OP_COMP_LESS_EQUAL,
    OP_COMP_EQUAL,
    OP_COMP_NOT_EQUAL,
    // Logical
    OP_LOGIC_AND,
    OP_LOGIC_OR,
    // Assignment
    OP_ASSIGN,
    OP_ADD_ASSIGN,
    OP_SUBTRACT_ASSIGN,
    OP_MULTIPLY_ASSIGN,
    OP_DIVIDE_ASSIGN,
    OP_MODULO_ASSIGN,
    OP_BITWISE_AND_ASSIGN,
    OP_BITWISE_OR_ASSIGN,
    OP_BITWISE_XOR_ASSIGN,
    OP_LEFT_SHIFT_ASSIGN,
    OP_RIGHT_SHIFT_ASSIGN,
    // Array Subscript
    OP_ARRAY_SUBSCRIPT,
    // Comma
    OP_COMMA
  };

  OpType m_operation = OpType::OP_ADDITION;
  UniqueExpression m_left_operand = nullptr;
  UniqueExpression m_right_operand = nullptr;

  BinaryOpNode() { m_node_type = NodeType::BINARY_EXPRESSION; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct TernaryOpNode : public ExpressionNode {
  UniqueExpression m_condition = nullptr;
  UniqueExpression m_true_expr = nullptr;
  UniqueExpression m_false_expr = nullptr;

  TernaryOpNode() { m_node_type = NodeType::TERNARY_EXPRESSION; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct MemberAccessNode : public ExpressionNode {
  enum class OpType {
    OP_DIRECT_MEM_ACCESS,   // .
    OP_INDIRECT_MEM_ACCESS  // ->
  };

  OpType m_access_type = OpType::OP_DIRECT_MEM_ACCESS;
  UniqueExpression m_expr = nullptr;
  std::string m_member;

  MemberAccessNode() { m_node_type = NodeType::MEMBER_ACCESS_EXPRESSION; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct CallNode : public ExpressionNode {
  std::string m_name = "";  // Name of function
  UniqueExpression m_callee = nullptr;
  std::vector<UniqueExpression> m_args;

  CallNode() { m_node_type = NodeType::FUNCTION_CALL; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct IdentifierNode : public ExpressionNode {
  std::string m_name = "";

  IdentifierNode() { m_node_type = NodeType::IDENTIFIER; }

  // Semantic Analysis
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct ConstantNode : public ExpressionNode {
  enum class ValType {
    SIGNED_CHAR,
    UNSIGNED_CHAR,
    SIGNED_SHORT,
    UNSIGNED_SHORT,
    SIGNED_INTEGER,
    UNSIGNED_INTEGER,
    SIGNED_LONG,
    UNSIGNED_LONG,
    FLOAT,
    DOUBLE,
    STRING
  };

  ValType m_val_type = ValType::SIGNED_INTEGER;
  LiteralType m_value;

  template <typename T>
  T get_val() const {
    return std::get<T>(m_value);
  }

  void update_val(LiteralType new_val) {
    if (std::holds_alternative<int8_t>(new_val)) {
      m_val_type = ValType::SIGNED_CHAR;
    }
    else if (std::holds_alternative<uint8_t>(new_val)) {
      m_val_type = ValType::UNSIGNED_CHAR;
    }
    else if (std::holds_alternative<int16_t>(new_val)) {
      m_val_type = ValType::SIGNED_SHORT;
    }
    else if (std::holds_alternative<uint16_t>(new_val)) {
      m_val_type = ValType::UNSIGNED_SHORT;
    }
    else if (std::holds_alternative<int32_t>(new_val)) {
      m_val_type = ValType::SIGNED_INTEGER;
    }
    else if (std::holds_alternative<uint32_t>(new_val)) {
      m_val_type = ValType::UNSIGNED_INTEGER;
    }
    else if (std::holds_alternative<int64_t>(new_val)) {
      m_val_type = ValType::SIGNED_LONG;
    }
    else if (std::holds_alternative<uint64_t>(new_val)) {
      m_val_type = ValType::UNSIGNED_LONG;
    }
    else if (std::holds_alternative<float>(new_val)) {
      m_val_type = ValType::FLOAT;
    }
    else if (std::holds_alternative<double>(new_val)) {
      m_val_type = ValType::DOUBLE;
    }
    else if (std::holds_alternative<std::string>(new_val)) {
      m_val_type = ValType::STRING;
    }

    m_value = new_val;
  }

  template <typename T>
  void update_val(T new_val) {
    if constexpr (std::same_as<T, int8_t>) {
      m_val_type = ValType::SIGNED_CHAR;
    }
    else if constexpr (std::same_as<T, uint8_t>) {
      m_val_type = ValType::UNSIGNED_CHAR;
    }
    else if constexpr (std::same_as<T, int16_t>) {
      m_val_type = ValType::SIGNED_SHORT;
    }
    else if constexpr (std::same_as<T, uint16_t>) {
      m_val_type = ValType::UNSIGNED_SHORT;
    }
    else if constexpr (std::same_as<T, int32_t>) {
      m_val_type = ValType::SIGNED_INTEGER;
    }
    else if constexpr (std::same_as<T, uint32_t>) {
      m_val_type = ValType::UNSIGNED_INTEGER;
    }
    else if constexpr (std::same_as<T, int64_t>) {
      m_val_type = ValType::SIGNED_LONG;
    }
    else if constexpr (std::same_as<T, uint64_t>) {
      m_val_type = ValType::UNSIGNED_LONG;
    }
    else if constexpr (std::same_as<T, float>) {
      m_val_type = ValType::FLOAT;
    }
    else if constexpr (std::same_as<T, double>) {
      m_val_type = ValType::DOUBLE;
    }
    else if constexpr (std::same_as<T, std::string>) {
      m_val_type = ValType::STRING;
    }
    else {
      static_assert(false);
    }

    m_value = new_val;
  }

  ConstantNode() { m_node_type = NodeType::CONSTANT; }

  // Semantic Analysis

  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

/////////////
// Statements

struct StatementNode : public Node {
  virtual ~StatementNode() = default;

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

 protected:
  StatementNode() = default;
};

struct LabelStmt : public StatementNode {
  std::string m_label = "";

  LabelStmt() { m_node_type = NodeType::LABEL_STATEMENT; }

  // Semantic Analysis
  virtual void find_labels(std::unordered_set<std::string> *labels) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct CaseStmt : public StatementNode {
  UniqueExpression m_expr = nullptr;

  CaseStmt() { m_node_type = NodeType::CASE_STATEMENT; }

  // Semantic Analysis
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct DefaultStmt : public StatementNode {
  DefaultStmt() { m_node_type = NodeType::DEFAULT_STATEMENT; }

  // Semantic Analysis
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct CompoundStmt : public StatementNode {
  std::vector<UniqueNode> m_stmts;  // Can be Statements or Declarations

  CompoundStmt() { m_node_type = NodeType::COMPOUND_STATEMENT; }

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct ExpressionStmt : public StatementNode {
  UniqueExpression m_expr = nullptr;

  ExpressionStmt() { m_node_type = NodeType::EXPRESSION_STATEMENT; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct IfStmt : public StatementNode {
  UniqueExpression m_condition = nullptr;
  UniqueStatement m_true_stmt = nullptr;
  UniqueStatement m_false_stmt = nullptr;

  IfStmt() { m_node_type = NodeType::IF_STATEMENT; }

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct SwitchStmt : public StatementNode {
  UniqueExpression m_expr = nullptr;
  UniqueStatement m_stmt = nullptr;

  SwitchStmt() { m_node_type = NodeType::SWITCH_STATEMENT; }

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct WhileStmt : public StatementNode {
  enum class WhileType { WHILE, DO };
  WhileType m_while_type = WhileType::WHILE;

  UniqueExpression m_condition = nullptr;
  UniqueStatement m_stmt = nullptr;

  WhileStmt() { m_node_type = NodeType::WHILE_STATEMENT; }

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct ForStmt : public StatementNode {
  UniqueNode m_init_clause = nullptr;
  UniqueExpression m_condition = nullptr;  // Will never be nullptr
  UniqueExpression m_iteration = nullptr;
  UniqueStatement m_stmt = nullptr;

  ForStmt() { m_node_type = NodeType::FOR_STATEMENT; }

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct ControlStmt : public StatementNode {
  enum class ControlType { BREAK, CONTINUE };
  ControlType m_control = ControlType::BREAK;

  ControlStmt() { m_node_type = NodeType::CONTROL_STATEMENT; }

  // Semantic Analysis
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct ReturnStmt : public StatementNode {
  UniqueExpression m_return_value = nullptr;

  ReturnStmt() { m_node_type = NodeType::RETURN_STATEMENT; }

  // Semantic Analysis

  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct GotoStmt : public StatementNode {
  std::string m_label;

  GotoStmt() { m_node_type = NodeType::GOTO_STATEMENT; }

  // Semantic Analysis
  virtual void resolve_identifiers(SemanticContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

///////////////
// Declarations

struct DeclarationNode : public Node {
  enum class StorageClassSpec { NONE, AUTO, REGISTER, STATIC, EXTERN, TYPEDEF };
  StorageClassSpec m_storage_spec = StorageClassSpec::NONE;
  SharedDataType m_data_type = nullptr;
  std::string m_identifier;

  virtual ~DeclarationNode() = default;

  // Semantic Analysis
  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void control_flow(FlowContext *context) override;

  void graph_gen_type(const void *parent_id, std::ostream &out) const;

 protected:
  DeclarationNode() = default;
};

struct VariableDeclaration : public DeclarationNode {
  UniqueExpression m_initializer = nullptr;

  VariableDeclaration() { m_node_type = NodeType::VARIABLE_DECLARATION; }

  // Semantic Analysis
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct FunctionDefinition : public DeclarationNode {
  // DataType will be of FunctionType
  std::vector<UniqueDeclaration> m_params;
  UniqueStatement m_body = nullptr;

  FunctionDefinition() { m_node_type = NodeType::FUNCTION_DECLARATION; }

  // Semantic Analysis
  std::unordered_set<std::string> labels;

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct ArrayDeclaration : public DeclarationNode {
  // TODO arrays can be initialized with array initializers
  UniqueExpression m_initializer = nullptr;
  UniqueExpression m_size = nullptr;

  ArrayDeclaration() { m_node_type = NodeType::ARRAY_DECLARATION; }

  // Semantic Analysis
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

struct TranslationUnit : public Node {
  std::vector<UniqueDeclaration> m_program;

  TranslationUnit() { m_node_type = NodeType::TRANSLATION_UNIT; }

  // Semantic Analysis

  virtual void find_labels(std::unordered_set<std::string> *labels) override;
  virtual void resolve_identifiers(SemanticContext *context) override;
  virtual SharedDataType resolve_types() override;
  virtual UniqueExpression constant_fold() override;
  virtual void control_flow(FlowContext *context) override;

  virtual std::string to_string() const override;
  virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
};

}  // namespace JCC
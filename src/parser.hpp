#pragma once

#include <memory>
#include <optional>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "lexer.hpp"

namespace JCC {

class Parser {
 private:
  void error(const std::string &p_error_message);
  void warning(const std::string &p_message);

  bool m_has_error = false;
  bool m_panic_mode = false;

 public:
  // Forward Decls
  struct DataType;
  struct Node;
  struct ExpressionNode;
  struct UnaryOpNode;
  struct BinaryOpNode;
  struct TernaryOpNode;
  struct CallNode;
  struct IdentifierNode;
  struct ConstantNode;
  struct StatementNode;
  struct LabelStmt;
  struct CompoundStmt;
  struct DeclarationNode;

  using SharedDataType = std::shared_ptr<DataType>;

  // Types
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

    bool is_const = false;
    bool is_restricted = false;
    bool is_volatile = false;

    bool m_is_signed = false;
    unsigned int m_size = 0;  // Size in bytes

    DataType() = default;
    DataType(TypeKind p_kind, bool p_is_signed, unsigned int p_size)
        : m_kind(p_kind), m_is_signed(p_is_signed), m_size(p_size) {}

    inline void set_qualifiers(const std::unordered_set<TypeQualifier> &qualifiers) {
      is_const = qualifiers.contains(TypeQualifier::CONST);
      is_restricted = qualifiers.contains(TypeQualifier::RESTRICT);
      is_volatile = qualifiers.contains(TypeQualifier::VOLATILE);
    }

    inline bool operator==(const DataType &p_rhs) {
      // Bools for debugging
      bool same_kind = m_kind == p_rhs.m_kind;
      bool same_sign = m_is_signed == p_rhs.m_is_signed;
      bool same_size = m_size == p_rhs.m_size;
      return same_kind && same_sign && same_size;
    }
    inline bool operator!=(const DataType &p_rhs) { return !(operator==(p_rhs)); }

    virtual SharedDataType clone() const { return std::make_shared<DataType>(*this); }
    virtual void graph_gen(const void *parent_id, const std::string &connection, const std::string &name,
                           std::ostream &out) const;
    void graph_gen(const void *parent_id, const void *child_id, const std::string &name, const std::string &connection,
                   std::ostream &out) const;
  };

  struct StructUnionType : public DataType {
    struct StructUnionField {
      enum class FieldType { STRUCT_FIELD, UNION_FIELD } field_type = FieldType::STRUCT_FIELD;
      std::string name;
      SharedDataType type;
    };
    std::vector<StructUnionField> fields;
    std::string name = "";  // if "" then it's anonymous
    unsigned int size = 0;
    unsigned int alignment = 0;

    StructUnionType() { m_kind = TypeKind::TYPE_INCOMPLETE; }

    virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
    virtual void graph_gen(const void *parent_id, const std::string &connection, const std::string &name,
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
    SharedDataType return_type;
    std::vector<SharedDataType> param_types;

    FunctionType() { m_kind = TypeKind::TYPE_FUNCTION; }

    virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
    virtual void graph_gen(const void *parent_id, const std::string &connection, const std::string &name,
                           std::ostream &out) const override;
  };

  struct PointerType : public DataType {
    SharedDataType m_base_type = nullptr;

    PointerType() { m_kind = TypeKind::TYPE_POINTER; }

    virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
    virtual void graph_gen(const void *parent_id, const std::string &connection, const std::string &name,
                           std::ostream &out) const override;
  };

  struct ArrayType : public DataType {
    SharedDataType m_base_type = nullptr;

    ArrayType() { m_kind = TypeKind::TYPE_ARRAY; }

    virtual SharedDataType clone() const override { return std::make_shared<DataType>(*this); }
    virtual void graph_gen(const void *parent_id, const std::string &connection, const std::string &name,
                           std::ostream &out) const override;
  };

  // Abstract Syntax Tree Nodes

  struct Node {
    enum class NodeType {
      NONE,
      UNARY_EXPRESSION,
      BINARY_EXPRESSION,
      TERNARY_EXPRESSION,
      FUNCTION_CALL,
      IDENTIFIER,
      CONSTANT,
      // Statements
      LABEL_STATEMENT,
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

    NodeType node_type = NodeType::NONE;
    size_t m_line = 0;
    size_t m_column_start = 0;

    virtual ~Node() = default;
    virtual bool is_expression() const { return false; }
    virtual std::string to_string() const = 0;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const = 0;
    void graph_gen(const void *parent_id, const void *child_id, const std::string &name, const std::string &connection,
                   std::ostream &out) const;
  };

  using UniqueNode = std::unique_ptr<Node>;

  // Expressions
  struct ExpressionNode : public Node {
    SharedDataType d_type;
    bool is_lvalue = false;

    virtual ~ExpressionNode() = default;
    virtual bool is_expression() const override { return true; }

    void graph_gen_type(std::ostream &out) const;

   protected:
    ExpressionNode() = default;
  };

  using UniqueExpression = std::unique_ptr<ExpressionNode>;

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
      OP_SIZEOF
    };

    OpType operation = OpType::OP_POSITIVE;
    UniqueExpression operand = nullptr;

    UnaryOpNode() { node_type = NodeType::UNARY_EXPRESSION; }

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
      // Comparison
      OP_COMP_GREATER,
      OP_COMP_LESS,
      OP_COMP_GREATER_EQUAL,
      OP_COMP_LESS_EQUAL,
      OP_COMP_EQUAL,
      OP_COMP_NOT_EQUAL,
      // Bitwise
      OP_BIT_AND,
      OP_BIT_OR,
      OP_BIT_XOR,
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
      // Member Access
      OP_DIRECT_MEM_ACCESS,   // .
      OP_INDIRECT_MEM_ACCESS  // ->
    };

    OpType operation = OpType::OP_ADDITION;
    UniqueExpression left_operand = nullptr;
    UniqueExpression right_operand = nullptr;

    BinaryOpNode() { node_type = NodeType::BINARY_EXPRESSION; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct TernaryOpNode : public ExpressionNode {
    UniqueExpression condition = nullptr;
    UniqueExpression true_expr = nullptr;
    UniqueExpression false_expr = nullptr;

    TernaryOpNode() { node_type = NodeType::TERNARY_EXPRESSION; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct CallNode : public ExpressionNode {
    std::string name = "";
    UniqueExpression callee = nullptr;
    std::vector<UniqueExpression> args;

    CallNode() { node_type = NodeType::FUNCTION_CALL; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct IdentifierNode : public ExpressionNode {
    std::string name = "";

    IdentifierNode() { node_type = NodeType::IDENTIFIER; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct ConstantNode : public ExpressionNode {
    enum class ValType { INTEGER, FLOAT, STRING };

    ValType val_type = ValType::INTEGER;
    LiteralType value;

    template <typename T>
    T get_val() const {
      return std::get<T>(value);
    }

    ConstantNode() { node_type = NodeType::CONSTANT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  // Statements
  struct StatementNode : public Node {
    virtual ~StatementNode() = default;
    virtual bool is_expression() const override { return false; }

   protected:
    StatementNode() = default;
  };

  using UniqueStatement = std::unique_ptr<StatementNode>;

  struct LabelStmt : public StatementNode {
    enum class LabelType { GOTO, CASE, DEFAULT };

    LabelType label_type = LabelType::GOTO;
    UniqueExpression expr = nullptr;  // Identifier for GOTO, Constant for Case
    UniqueStatement stmt = nullptr;

    LabelStmt() { node_type = NodeType::LABEL_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct CompoundStmt : public StatementNode {
    std::vector<UniqueNode> stmts;  // Can be Statements or Declarations

    CompoundStmt() { node_type = NodeType::COMPOUND_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct ExpressionStmt : public StatementNode {
    UniqueExpression expr = nullptr;

    ExpressionStmt() { node_type = NodeType::EXPRESSION_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct IfStmt : public StatementNode {
    UniqueExpression condition = nullptr;
    UniqueStatement true_stmt = nullptr;
    UniqueStatement false_stmt = nullptr;

    IfStmt() { node_type = NodeType::IF_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct SwitchStmt : public StatementNode {
    UniqueExpression expr = nullptr;
    UniqueStatement stmt = nullptr;

    SwitchStmt() { node_type = NodeType::SWITCH_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct WhileStmt : public StatementNode {
    enum class WhileType { WHILE, DO };
    WhileType while_type = WhileType::WHILE;
    UniqueExpression condition = nullptr;
    UniqueStatement stmt = nullptr;

    WhileStmt() { node_type = NodeType::WHILE_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct ForStmt : public StatementNode {
    UniqueNode init_clause = nullptr;  // TODO this can be expression, or declaration
    UniqueExpression condition = nullptr;
    UniqueExpression iteration = nullptr;
    UniqueStatement stmt = nullptr;

    ForStmt() { node_type = NodeType::FOR_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct ControlStmt : public StatementNode {
    enum class ControlType { BREAK, CONTINUE };
    ControlType control = ControlType::BREAK;

    ControlStmt() { node_type = NodeType::CONTROL_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct ReturnStmt : public StatementNode {
    UniqueExpression return_value = nullptr;

    ReturnStmt() { node_type = NodeType::RETURN_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct GotoStmt : public StatementNode {
    UniqueExpression identifier;  // Must be an identifier

    GotoStmt() { node_type = NodeType::GOTO_STATEMENT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  // Declarations
  struct DeclarationNode : public Node {
    enum class StorageClassSpec { NONE, AUTO, REGISTER, STATIC, EXTERN, TYPEDEF };
    StorageClassSpec m_storage_spec = StorageClassSpec::NONE;
    SharedDataType m_data_type = nullptr;
    std::string identifier;

    virtual ~DeclarationNode() = default;
    virtual bool is_expression() const override { return false; }
    void graph_gen_type(std::ostream &out) const;

   protected:
    DeclarationNode() = default;
  };

  using UniqueDeclaration = std::unique_ptr<DeclarationNode>;

  struct VariableDeclaration : public DeclarationNode {
    UniqueExpression initializer = nullptr;

    VariableDeclaration() { node_type = NodeType::VARIABLE_DECLARATION; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct FunctionDefinition : public DeclarationNode {
    // DataType will be of FunctionType
    std::vector<UniqueDeclaration> params;
    UniqueStatement body = nullptr;

    FunctionDefinition() { node_type = NodeType::FUNCTION_DECLARATION; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct ArrayDeclaration : public DeclarationNode {
    // TODO arrays can be initialized with array initializers
    UniqueExpression initializer = nullptr;
    UniqueExpression size = nullptr;

    ArrayDeclaration() { node_type = NodeType::ARRAY_DECLARATION; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

  struct TranslationUnit : public Node {
    std::vector<UniqueDeclaration> program;

    TranslationUnit() { node_type = NodeType::TRANSLATION_UNIT; }
    virtual std::string to_string() const override;
    virtual void graph_gen(const void *parent_id, const std::string &connection, std::ostream &out) const override;
  };

 private:
  template <typename T>
    requires std::is_base_of_v<Node, T>
  T *alloc_node() {
    T *node = new T();

    node->m_line = m_previous_tok.m_line;
    node->m_column_start = m_previous_tok.m_column_start;

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
  UniqueStatement parse_label_statement();
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
  SharedDataType parse_struct_union(Parser::DataType::TypeKind p_kind);
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
  struct ParserContext {
    std::unordered_map<std::string, SharedDataType> m_type_symbol_table;
    std::unordered_map<std::string, int> m_enumeration_constants;

    std::unique_ptr<ParserContext> m_previous_context = nullptr;
  };

  std::unique_ptr<ParserContext> m_current_context;
  inline void push_context() {
    std::unique_ptr<ParserContext> next_context = std::make_unique<ParserContext>();
    next_context->m_previous_context = std::move(m_current_context);
    m_current_context = std::move(next_context);
  }
  inline void pop_context() { m_current_context = std::move(m_current_context->m_previous_context); }

  inline void push_type(const std::string &p_name, SharedDataType p_new_type) {
    m_current_context->m_type_symbol_table[p_name] = p_new_type;
  }

  // Gets a type from the current context to allow for shadowing of types in previous context
  inline SharedDataType get_type_cur_context(const std::string &p_name) {
    auto it = m_current_context->m_type_symbol_table.find(p_name);
    if (it == m_current_context->m_type_symbol_table.end()) {
      return nullptr;
    }
    return it->second;
  }

  inline SharedDataType get_type(const std::string &p_name) {
    ParserContext *context = m_current_context.get();
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
    ParserContext *context = m_current_context.get();
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
    m_current_context->m_enumeration_constants[p_name] = value;
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

  void graph_gen(std::ostream &out, const Node *node) const;
};

}  // namespace JCC
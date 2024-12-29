#include "parser.h"
#include "ansi_colors.h"
#include "src/nodes.h"
#include "src/token.h"

#include <stdarg.h>
#include <stdlib.h>

/* ERROR REPORTING */

static void parser_error(struct parser* p, const char* msg) {
  struct token *t = p->tokens[p->current];

  fprintf(stderr, ANSI_COLOR_RED "Error: " ANSI_COLOR_RESET "%s %ld:%ld %s\n%s\n", 
    t->filename, 
    t->line, 
    t->column, 
    t->lexeme,
    msg
  );

  p->error = true;
  p->checked_error = false;
}

static void parser_warning(struct parser* p, const char* msg) {
  struct token *t = p->tokens[p->current];

  fprintf(stderr, ANSI_COLOR_YELLOW "Warning: " ANSI_COLOR_RESET  "%s %ld:%ld %s\n%s\n", 
    t->filename, 
    t->line, 
    t->column, 
    t->lexeme,
    msg
  );
  
}

/* END ERROR REPORTING */


// Typedefs

typedef struct parser parser;
typedef struct token token_s; 
typedef struct ast_node ast_node;


/* Parsing utility functions */
static inline token_s *peek(parser *p);

static inline bool at_end(parser *p) {
  return peek(p)->type == EOFF;
}

static inline token_s *peek(parser *p) {
  return p->tokens[p->current];
}

static inline token_s *previous(parser *p) {
  return p->tokens[p->current-1];
}

static inline token_s *get(parser *p) {
  if(!at_end(p)) p->current++;
  return previous(p);
}

static inline void unget(parser *p) {
  p->current--;
}

static inline bool check(parser *p, enum token_type type) {
  return at_end(p) ? false : (peek(p)->type == type);
} 

static inline token_s *consume(parser *p, enum token_type type, const char* msg) {
  if(check(p, type)) return get(p);

  parser_error(p, msg);
  return NULL;
}

static bool __match(parser *p, enum token_type *types) {
  enum token_type cur;
  while((cur = *types) != EOFF) {
    if(check(p, cur)) {
      get(p);
      return true;
    }
    types++;
  }

  return false;
} 

#define match(p, ...) __match(p, (enum token_type[]){__VA_ARGS__, EOFF})

// Puts the parser in a good position to be able to continue parsing after an error occurred
static void synchronize(parser *p) {
  get(p);

  while(!at_end(p)) {
    if(previous(p)->type == SEMICOLON) return;

    switch (peek(p)->type) {
      case AUTO:
      case BREAK:
      case CASE:
      case CHAR:
      case CONST:
      case CONTINUE:
      case DEFAULT:
      case DO:
      case DOUBLE:
      case ELSE:
      case ENUM:
      case EXTERN:
      case FLOAT:
      case FOR:
      case GOTO:
      case IF:
      case INLINE:
      case INT:
      case LONG:
      case REGISTER:
      case RESTRICT:
      case RETURN:
      case SHORT:
      case SIGNED:
      case SIZEOF:
      case STATIC:
      case STRUCT:
      case SWITCH:
      case TYPEDEF:
      case UNION:
      case UNSIGNED:
      case VOID:
      case VOLATILE:
      case WHILE:
        return;
      default:
        get(p);
        break;
    }
  }
}

/* END PARSING UTILITY FUNCTIONS */

/* BEGIN FACTORY FUNCTIONS */

static void *make_node(int size, token_s *tok, enum node_type type) {
  ast_node *node = malloc(size);
  node->line = tok->line;
  node->column = tok->column;
  node->type = type;
  return node;
}

static inline ast_node *make_binary_expression(token_s *tok, enum node_type type, ast_node *left, ast_node *right) {
  struct binary_expression_node *node = make_node(sizeof(struct binary_expression_node), tok, type);

  node->left = left;
  node->right = right;

  return (ast_node*)node;
}

static ast_node *binary_expression_factory(token_s *tok, ast_node *left, ast_node *right) {
  switch (tok->type) {
    case PLUS:
      return make_binary_expression(tok, ADD_EXPRESSION, left, right);
    case MINUS:
      return make_binary_expression(tok, SUBTRACT_EXPRESSION, left, right);
    case STAR:
      return make_binary_expression(tok, MULTIPLY_EXPRESSION, left, right);
    case SLASH:
      return make_binary_expression(tok, DIVIDE_EXPRESSION, left, right);
    case PERCENT:
      return make_binary_expression(tok, MODULO_EXPRESSION, left, right);
    case AMPERSAND_AMPERSAND:
      return make_binary_expression(tok, LOGICAL_AND_EXPRESSION, left, right);
    case PIPE_PIPE:
      return make_binary_expression(tok, LOGICAL_OR_EXPRESSION, left, right);
    case EQUAL_EQUAL:
      return make_binary_expression(tok, EQUALITY_EXPRESSION, left, right);
    case BANG_EQUAL:
      return make_binary_expression(tok, INEQUALITY_EXPRESSION, left, right);
    case LESS:
      return make_binary_expression(tok, LESS_EXPRESSION, left, right);
    case GREATER:
      return make_binary_expression(tok, GREATER_EXPRESSION, left, right);
    case LESS_EQUAL:
      return make_binary_expression(tok, LESS_EQUAL_EXPRESSION, left, right);
    case GREATER_EQUAL:
      return make_binary_expression(tok, GREATER_EQUAL_EXPRESSION, left, right);
    case SHIFT_LEFT:
      return make_binary_expression(tok, LEFT_SHIFT_EXPRESSION, left, right);
    case SHIFT_RIGHT:
      return make_binary_expression(tok, RIGHT_SHIFT_EXPRESSION, left, right);
    case PIPE:
      return make_binary_expression(tok, BITWISE_OR_EXPRESSION, left, right);
    case AMPERSAND:
      return make_binary_expression(tok, BITWISE_AND_EXPRESSION, left, right);
    case CARET:
      return make_binary_expression(tok, BITWISE_XOR_EXPRESSION, left, right);
    case EQUAL:
      return make_binary_expression(tok, ASSIGN_EXPRESSION, left, right);
    case PLUS_EQUAL:
      return make_binary_expression(tok, ADD_ASSIGN_EXPRESSION, left, right);
    case MINUS_EQUAL:
      return make_binary_expression(tok, SUBTRACT_ASSIGN_EXPRESSION, left, right);
    case STAR_EQUAL:
      return make_binary_expression(tok, MULTIPLY_ASSIGN_EXPRESSION, left, right);
    case SLASH_EQUAL:
      return make_binary_expression(tok, DIVIDE_ASSIGN_EXPRESSION, left, right);
    case PERCENT_EQUAL:
      return make_binary_expression(tok, MODULO_ASSIGN_EXPRESSION, left, right);
    case AMPERSAND_EQUAL:
      return make_binary_expression(tok, BITWISE_AND_ASSIGN_EXPRESSION, left, right);
    case PIPE_EQUAL:
      return make_binary_expression(tok, BITWISE_OR_ASSIGN_EXPRESSION, left, right);
    case CARET_EQUAL:
      return make_binary_expression(tok, BITWISE_XOR_ASSIGN_EXPRESSION, left, right);
    case SHIFT_LEFT_EQUAL:
      return make_binary_expression(tok, LEFT_SHIFT_ASSIGN_EXPRESSION, left, right);
    case SHIFT_RIGHT_EQUAL:
      return make_binary_expression(tok, RIGHT_SHIFT_ASSIGN_EXPRESSION, left, right);
    case COMMA:
      return make_binary_expression(tok, COMMA_EXPRESSION, left, right);
    default:
      fprintf(stderr,"Fatal Error: %s:%d %s - Unknown binary expression\n", __FILE__,  __LINE__, __FUNCTION__);
      exit(1);
  }
}

static inline ast_node *make_unary_expression(token_s *tok, enum node_type type, ast_node *operand) {
  struct unary_expression_node *node = make_node(sizeof(struct unary_expression_node), tok, type);

  node->type = type;
  node->line = tok->line;
  node->column = tok->column;
  node->operand = operand;

  return (ast_node*)node;
}

static ast_node *unary_expression_factory(token_s *tok, ast_node *operand) {
  switch (tok->type) {
    case PLUS:
      return make_unary_expression(tok, POSITIVE_EXPRESSION, operand);
    case MINUS:
      return make_unary_expression(tok, NEGATION_EXPRESSION, operand);
    case BANG:
      return make_unary_expression(tok, LOGICAL_NOT_EXPRESSION, operand);
    case TILDE:
      return make_unary_expression(tok, BITWISE_NOT_EXPRESSION, operand);
    case STAR:
      return make_unary_expression(tok, DEREFERENCE_EXPRESSION, operand);
    case AMPERSAND:
      return make_unary_expression(tok, ADDRESS_OF_EXPRESSION, operand);
    case SIZEOF:
      return make_unary_expression(tok, SIZE_OF_EXPRESSION, operand);
    case PLUS_PLUS:
      return make_unary_expression(tok, PREFIX_INCREMENT_EXPRESSION, operand);
    case MINUS_MINUS:
      return make_unary_expression(tok, PREFIX_DECREMENT_EXPRESSION, operand);
    default:
      fprintf(stderr,"Fatal Error: %s:%d %s - Unknown unary expression\n", __FILE__,  __LINE__, __FUNCTION__);
      exit(1);
  }
}

/* END FACTORY FUNCTIONS */

/* BEGIN EXPRESSION PARSING FUNCTIONS */

static ast_node *assign(parser *p);
static ast_node *ternary(parser *p);
static ast_node *logical_or(parser *p);
static ast_node *logical_and(parser *p);
static ast_node *bitwise_or(parser *p);
static ast_node *bitwise_xor(parser *p);
static ast_node *bitwise_and(parser *p);
static ast_node *equality(parser *p);
static ast_node *comparision(parser *p);
static ast_node *shift(parser *p);
static ast_node *term(parser *p);
static ast_node *factor(parser *p);
static ast_node *prefix(parser *p);
static ast_node *postfix(parser *p);
static ast_node *primary(parser *p);
static ast_node *arguments(parser *p);


static ast_node *expression(parser *p) {
  ast_node *expr = assign(p);
  while(match(p, COMMA)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, assign(p));
  }
  return expr;
}

static ast_node *assign(parser *p) {
  ast_node *expr = ternary(p);
  while(match(p, EQUAL, SHIFT_RIGHT_EQUAL, SHIFT_LEFT_EQUAL, PLUS_EQUAL,
                MINUS_EQUAL, STAR_EQUAL, SLASH_EQUAL, PERCENT_EQUAL,
                AMPERSAND_EQUAL, PIPE_EQUAL, CARET_EQUAL)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, assign(p));
  }
  return expr;
}

static ast_node *ternary(parser *p) {
  // expr QUESTION expr1 COLON expr2
  ast_node *expr = logical_or(p);
  while(match(p, QUESTION)) {
    token_s *op = previous(p);
    ast_node *expr1 = expression(p);

    consume(p, COLON, "Invalid ternary expression");

    struct ternary_expression_node *tern = make_node(sizeof(struct ternary_expression_node), op, TERNARY_EXPRESSION);
    tern->condition = expr;
    tern->true_node = expr1;
    tern->false_node = ternary(p);
    expr = (ast_node*)tern;
  }
  return expr;
}

static ast_node *logical_or(parser *p) {
  ast_node *expr = logical_and(p);
  while(match(p, PIPE_PIPE)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, logical_and(p));
  }
  return expr;
}

static ast_node *logical_and(parser *p) {
  ast_node *expr = bitwise_or(p);
  while(match(p, AMPERSAND_AMPERSAND)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, bitwise_or(p));
  }
  return expr;
}

static ast_node *bitwise_or(parser *p) {
  ast_node *expr = bitwise_xor(p);
  while(match(p, PIPE)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, bitwise_xor(p));
  }
  return expr;
}

static ast_node *bitwise_xor(parser *p) {
  ast_node *expr = bitwise_and(p);
  while(match(p, CARET)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, bitwise_and(p));
  }
  return expr;
}

static ast_node *bitwise_and(parser *p) {
  ast_node *expr = equality(p);
  while(match(p, AMPERSAND)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, equality(p));
  }
  return expr;
}

static ast_node *equality(parser *p) {
  ast_node *expr = comparision(p);
  while(match(p, EQUAL_EQUAL, BANG_EQUAL)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, comparision(p));
  }
  return expr;
}

static ast_node *comparision(parser *p) {
  ast_node *expr = shift(p);
  while(match(p, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, shift(p));
  }
  return expr;
}

static ast_node *shift(parser *p) {
  ast_node *expr = term(p);
  while(match(p, SHIFT_LEFT, SHIFT_RIGHT)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, term(p));
  }
  return expr;
}

static ast_node *term(parser *p) {
  ast_node *expr = factor(p);
  while(match(p, PLUS, MINUS)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, factor(p));
  }
  return expr;
}

static ast_node *factor(parser *p) {
  ast_node *expr = prefix(p);
  while(match(p, STAR, SLASH, PERCENT)) {
    token_s *op = previous(p);
    expr = binary_expression_factory(op, expr, prefix(p));
  }
  return expr;
}

static ast_node *prefix(parser *p) {
  // TODO casts have the same presedence as these so it should be evaluated at this time
  if(match(p, PLUS_PLUS, MINUS_MINUS, PLUS, MINUS, BANG, TILDE, STAR, AMPERSAND, SIZEOF)) {
    token_s *op = previous(p);
    return unary_expression_factory(op, prefix(p));
  }
  return postfix(p);
}

static ast_node *postfix(parser *p) {
  // TODO need to parse ( type-name ) {initializer-list }
  // Currently primary() will see ( and parse that into an expression

  ast_node* expr = primary(p);
  while(match(p, LEFT_PAREN, LEFT_BRACKET,
                DOT, PLUS_PLUS, MINUS_MINUS, ARROW)) {
    token_s *op = previous(p);

    switch(op->type) {
      case LEFT_BRACKET: {
        ast_node *index = expression(p);
        consume(p, RIGHT_BRACKET, "Missing ']'");
        expr = make_binary_expression(op, ARRAY_ACCESS_EXPRESSION, expr, index);
        break;
      }
      case LEFT_PAREN: {
        ast_node *args = NULL;
        if(peek(p)->type != RIGHT_PAREN) {
          args = arguments(p);
        }
        consume(p, RIGHT_PAREN, "Missing ')'");
        expr = make_binary_expression(op, FUNCTION_CALL_EXPRESSION, expr, args);
        break;
      }
      case ARROW:
      case DOT:{
        token_s *ident = consume(p, IDENTIFIER, "Expected Identifier");
        enum node_type type = op->type == DOT ? DOT_ACCESS_EXPRESSION : ARROW_ACCESS_EXPRESSION;
        struct struct_access_node *temp_expr = make_node(sizeof(struct struct_access_node), op, type);
        temp_expr->storage = expr;
        temp_expr->identifier = ident->lexeme;
        expr = (ast_node *)temp_expr;
        break;
      }
      case PLUS_PLUS:
        return make_unary_expression(op, POSTFIX_INCREMENT_EXPRESSION, expr);
      case MINUS_MINUS:
        return make_unary_expression(op, POSTFIX_DECREMENT_EXPRESSION, expr);
      default:
        // Under correct program flow, it can never reach here
        // If program reaches here that means it was coded wrong
        fprintf(stderr,"Fatal Error: %s:%d %s - Unknown postfix expression\n", __FILE__,  __LINE__, __FUNCTION__);
        exit(1);
    }
  }
  return expr;
}

static ast_node *arguments(parser *p) {
  token_s *prev = previous(p);
  struct argument_list* args = make_node(sizeof(struct argument_list), prev, ARGUMENT_LIST);

  args->args = malloc(sizeof(ast_node**));
  args->num_args = 1;

  args->args[0] = assign(p);
  while(match(p, COMMA)) {
    args->num_args++;
    args->args = realloc(args->args, args->num_args * sizeof(ast_node**));
    if(!args->args) {
      fprintf(stderr,"Fatal Error: %s:%d %s - Out of Memory\n", __FILE__,  __LINE__, __FUNCTION__);
      exit(1);
    }
    args->args[args->num_args - 1] = assign(p);
  }

  return (ast_node*)args;
}

static ast_node *primary(parser *p) {
  token_s *cur = peek(p);

  if(match(p, NUMBER_LITERAL, FLOAT_LITERAL, STRING_LITERAL)) {
    struct literal_expression_node *node = make_node(sizeof(struct literal_expression_node), cur, 0);
    switch(cur->type) {
      case NUMBER_LITERAL:
        node->type = INTEGER_LITERAL_EXPRESSION;
        break;
      case FLOAT_LITERAL:
        node->type = FLOAT_LITERAL_EXPRESSION;
        break;
      case STRING_LITERAL:
        node->type = STRING_LITERAL_EXPRESSION;
        break;
      default:
        break;
    }
    node->lit = cur->literal;

    return (ast_node*)node;
  }
  if(match(p, IDENTIFIER)) {
    struct identifier_expression_node *node = make_node(sizeof(struct identifier_expression_node), cur, IDENTIFIER_EXPRESSION);
    node->identifier = cur->lexeme;

    return (ast_node*)node;
  }
  if(match(p, LEFT_PAREN)) {
    ast_node *expr = expression(p);
    consume(p, RIGHT_PAREN, "Expected ')'");
    return expr;
  }

  return NULL;
}


/* END EXPRESSION PARSING FUNCTIONS */

/* BEGIN DECLARATION PARSING FUNCTIONS */

static ast_node *declaration_specifier(parser *p);
static ast_node *declaration_init_list(parser *p);
static ast_node *declaration_init_declarator(parser *p);
static ast_node *declaration_declarator(parser *p);


static ast_node *declaration(parser *p) {
  ast_node *spec = declaration_specifier(p);
  struct decl_node *node = make_node(sizeof(struct decl_node), previous(p), DECLARATION);
  node->specifiers = spec;
  node->init_list = declaration_init_list(p);

  consume(p, SEMICOLON, "Missing ';'");

  return (ast_node *)node;
}

static ast_node *declaration_specifier(parser *p) {
  // TODO

  /*
  while(storage_class_spec || type_spec || type_qualifier || function_specifier) {
    append specifier
    TODO data structure for this
  }

  storage_class_spec:
    typedef, extern, static, auto, register

  type-specifier:
    void, char, short, int, long, float, double, signed, unsigned,
    struct_or_union spec, enum spec, typedef_name

  function specifiers:
    inline
  */

  return NULL;
}

static ast_node *declaration_init_list(parser *p) {
  ast_node *node = declaration_init_declarator(p);
  while(match(p, COMMA)) {
    ast_node *init_decl = declaration_init_declarator(p);

    // TODO
  }

  return node;
}

static ast_node *declaration_init_declarator(parser *p) {
  ast_node *decl = declaration_declarator(p);
  if(match(p , EQUAL)) {
    ast_node *initializer = NULL; // TODO
  }

  return decl;
}

static ast_node *declaration_declarator(parser *p) {
  // TODO page 126 of open-std spec for c99
  // https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
  return NULL;
}

/* END DECLARATION PARSING FUNCTIONS */

/* BEGIN STATEMENT PARSING FUNCTIONS*/

static ast_node *labeled_statement(parser *p);
static ast_node *compound_statement(parser *p);

static ast_node *statement(parser *p) {
  if(match(p, SEMICOLON)) return NULL;

  // labeled statement
  if(match(p, IDENTIFIER, CASE, DEFAULT)) {
    return labeled_statement(p);
  }
  // Compound statement
  if(match(p, LEFT_BRACE)) {
    return compound_statement(p);
  }
}

static ast_node *labeled_statement(parser *p) {
  token_s *prev = previous(p);
  switch(prev->type) {
    case IDENTIFIER: 
      if(match(p, COLON)) {
        struct stmt_label_node *node = make_node(sizeof(struct stmt_label_node), prev, LABEL_STATEMENT);
        node->identifier = prev->lexeme;
        return (ast_node*)node;
      } else {
        // Expression Statement
        unget(p);
        struct stmt_expression_node *node = make_node(sizeof(struct stmt_expression_node), prev, EXPR_STATEMENT);
        node->expr = expression(p);
        consume(p, SEMICOLON, "Expected ';' in Statement");
        return (ast_node*)node;
      }
    case CASE:
      // TODO constant expression
  }
}


/* END STATEMENT PARSING FUNCTIONS*/

ast_node *parse(token_s **tokens, int64_t tokens_size) {
  parser p = {
    .tokens = tokens,
    .tokens_size = tokens_size,

    .current = 0,

    .error = false,
    .checked_error = false
  };


  while(!at_end(&p)) {
    ast_node *expr = expression(&p);
    if(p.error && !p.checked_error) {
      p.checked_error = true;
      synchronize(&p);
    }
    print_node(expr);
    destroy_node(expr);
  }

  return NULL;
}

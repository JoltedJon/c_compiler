#pragma once

#include "ast.hpp"

namespace JCC {

// Semantic Analysis Context
struct SemanticContext {
  std::unordered_map<std::string, SharedDataType> identifiers;
  std::unordered_set<std::string> labels;
  SemanticContext *previous_context = nullptr;

  SharedDataType get_identifier_type(const std::string &identifier);
  void push_identifier(const std::string &identifier, SharedDataType type);

  // Static members

  static std::unordered_map<std::string, SharedDataType> base_types;
  static int num_errors;
  static void error(const Node *node, const std::string &message);
  static bool has_error() { return num_errors > 0; }
};

struct FlowContext {
  SharedDataType return_type = nullptr;
  int in_loop = 0;
  int in_switch = 0;
  bool has_return = false;
};

}  // namespace JCC

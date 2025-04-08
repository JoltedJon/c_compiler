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
  static bool has_error;
  static void error(const Node *node, const std::string &message);
};

}  // namespace JCC

#pragma once

#include <iostream>

#include "../ast.hpp"

namespace JCC {

void graph_gen(std::ostream &out, const Node *node);

}
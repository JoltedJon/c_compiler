#pragma once
#include <llvm-18/llvm/IR/LLVMContext.h>
#include <llvm-18/llvm/IR/Module.h>

#include <memory>

namespace JCC {

struct codegen_context {
  static llvm::LLVMContext* llvm_context;
  static llvm::Module* llvm_module;
};

}  // namespace JCC
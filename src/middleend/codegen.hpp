#pragma once

#include <llvm-18/llvm/IR/BasicBlock.h>
#include <llvm-18/llvm/IR/IRBuilder.h>
namespace JCC {

struct codegen_context {
  static std::unique_ptr<llvm::LLVMContext> llvm_context;
  static std::unique_ptr<llvm::IRBuilder<>> llvm_builder;
  static std::unique_ptr<llvm::Module> llvm_module;
  static std::unordered_map<std::string, llvm::AllocaInst *> named_values;
  static std::unordered_map<std::string, llvm::BasicBlock *> labeled_blocks;

  static llvm::BasicBlock *break_dest;
  static llvm::BasicBlock *continue_dest;
};

}  // namespace JCC
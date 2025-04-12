
#include <llvm-18/llvm/ADT/APFloat.h>
#include <llvm-18/llvm/ADT/APInt.h>
#include <llvm-18/llvm/Analysis/CGSCCPassManager.h>
#include <llvm-18/llvm/Analysis/LoopAnalysisManager.h>
#include <llvm-18/llvm/IR/BasicBlock.h>
#include <llvm-18/llvm/IR/Constant.h>
#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/IR/DerivedTypes.h>
#include <llvm-18/llvm/IR/Function.h>
#include <llvm-18/llvm/IR/IRBuilder.h>
#include <llvm-18/llvm/IR/Instruction.h>
#include <llvm-18/llvm/IR/Instructions.h>
#include <llvm-18/llvm/IR/LLVMContext.h>
#include <llvm-18/llvm/IR/Module.h>
#include <llvm-18/llvm/IR/PassInstrumentation.h>
#include <llvm-18/llvm/IR/PassManager.h>
#include <llvm-18/llvm/IR/Type.h>
#include <llvm-18/llvm/IR/Value.h>
#include <llvm-18/llvm/IR/Verifier.h>
#include <llvm-18/llvm/Passes/PassBuilder.h>
#include <llvm-18/llvm/Passes/StandardInstrumentations.h>
#include <llvm-18/llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm-18/llvm/Transforms/Scalar/GVN.h>
#include <llvm-18/llvm/Transforms/Scalar/Reassociate.h>
#include <llvm-18/llvm/Transforms/Scalar/SimplifyCFG.h>

#include <cstdint>
#include <format>
#include <iostream>
#include <memory>
#include <utility>

#include "../ansi_colors.hpp"
#include "../ast.hpp"

namespace JCC {

template <class T>
using Unique = std::unique_ptr<T>;

// Core functionality
static Unique<llvm::LLVMContext> llvm_context;
static Unique<llvm::IRBuilder<>> llvm_builder;
static Unique<llvm::Module> llvm_module;

// Optimization Objects
static Unique<llvm::FunctionPassManager> llvm_fpm;
static Unique<llvm::LoopAnalysisManager> llvm_lam;
static Unique<llvm::FunctionAnalysisManager> llvm_fam;
static Unique<llvm::CGSCCAnalysisManager> llvm_cgam;
static Unique<llvm::ModuleAnalysisManager> llvm_mam;
static Unique<llvm::PassInstrumentationCallbacks> llvm_pic;
static Unique<llvm::StandardInstrumentations> llvm_si;

// Codegen Specific data
static std::unordered_map<std::string, llvm::AllocaInst *> named_values;
static std::unordered_map<std::string, llvm::BasicBlock *> labeled_blocks;
static llvm::BasicBlock *break_dest;
static llvm::BasicBlock *continue_dest;

void log_error(const std::string &message) { std::cerr << message << std::endl; }

#define fatal_error(p_error_message)                                                                                \
  do {                                                                                                              \
    std::cerr << ANSI_COLOR_RED << "Fatal Error - Please Report Bug: " << ANSI_COLOR_RESET << "\n\t"                \
              << __PRETTY_FUNCTION__ << ":" << __LINE__ << "\n\tllvm generation: " << p_error_message << std::endl; \
    std::abort();                                                                                                   \
  } while (0)

llvm::Type *map_to_LLVM_type(SharedDataType p_type) {
  switch (p_type->m_kind) {
    case DataType::TypeKind::TYPE_VOID:
      return llvm::Type::getVoidTy(*llvm_context);
    case DataType::TypeKind::TYPE_INT: {
      uint32_t num_bits = p_type->m_size * 8;
      return llvm::IntegerType::get(*llvm_context, num_bits);
    }
    case DataType::TypeKind::TYPE_FLOAT: {
      if (p_type->m_size == 4) {
        return llvm::Type::getFloatTy(*llvm_context);
      }
      else {
        return llvm::Type::getDoubleTy(*llvm_context);
      }
    }
    case DataType::TypeKind::TYPE_STRUCT: {
      // TODO struct typing
      fatal_error("Struct type mapping to LLVM types not yet implemented");
    }
    case DataType::TypeKind::TYPE_UNION: {
      // TODO Union typing
      fatal_error("Union type mapping to LLVM types not yet implemented");
    }
    case DataType::TypeKind::TYPE_FUNCTION: {
      std::vector<llvm::Type *> param_types;
      FunctionType *func = static_cast<FunctionType *>(p_type.get());
      for (auto &param_type : func->m_param_types) {
        param_types.push_back(map_to_LLVM_type(param_type));
      }

      if (param_types.empty()) {
        return llvm::FunctionType::get(map_to_LLVM_type(func->m_return_type), false);
      }
      return llvm::FunctionType::get(map_to_LLVM_type(func->m_return_type), param_types, false);
    }
    case DataType::TypeKind::TYPE_ENUM: {
      // Enum type should be eliminated by this point
      fatal_error("Enum type mapping to LLVM types not yet implemented");
    }
    case DataType::TypeKind::TYPE_POINTER: {
      // Don't care about underlying pointer type, get an opaque pointer
      return llvm::PointerType::getUnqual(*llvm_context);
    }
    case DataType::TypeKind::TYPE_ARRAY: {
      ArrayType *arr = static_cast<ArrayType *>(p_type.get());
      if (arr->m_size == 0) {
        fatal_error("Variable Length arrays not yet implemented");
      }
      return llvm::ArrayType::get(map_to_LLVM_type(arr->m_base_type), arr->m_size);
    }
    default:
      log_error("Unknown Data Type");
      return nullptr;
  }
}

bool codegen_node(Node *node) {
  StatementNode *stmt = dynamic_cast<StatementNode *>(node);
  if (stmt) {
    stmt->codegen();
  }
  else {
    DeclarationNode *decl = dynamic_cast<DeclarationNode *>(node);
    if (!decl) return false;
    decl->codegen();
  }
  return true;
}

llvm::AllocaInst *alloc_variable(llvm::Function *func, llvm::Type *type, llvm::Value *array_size,
                                 const std::string &name) {
  llvm::IRBuilder<> tmp_builder(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp_builder.CreateAlloca(type, array_size, name);
}

llvm::Value *UnaryOpNode::codegen() {
  // Op Positive should not exist at this point due to constant folding
  assert(m_operation != OpType::OP_POSITIVE);

  if (m_operation == OpType::OP_SIZEOF) {
    fatal_error("Sizeof not yet implemented");
  }

  if (m_operation == OpType::OP_ADDRESS_OF) {
    m_operand->m_is_lvalue = true;
    return m_operand->codegen();
  }

  // Signal to identifier node (if present) that it should do a load
  m_operand->m_is_lvalue = false;

  llvm::Value *o = m_operand->codegen();
  if (o == nullptr) return nullptr;

  switch (m_operation) {
    case OpType::OP_POSITIVE:
      return o;
    case OpType::OP_NEGATIVE: {
      if (o->getType()->isFloatingPointTy()) {
        llvm::Value *zero = llvm::ConstantFP::get(o->getType(), 0.0);
        return llvm_builder->CreateFSub(zero, o, "fnegtmp");
      }
      else {
        llvm::Value *zero = llvm::ConstantInt::get(o->getType(), 0);
        return llvm_builder->CreateSub(zero, o, "negtmp");
      }
    }
    case OpType::OP_INCREMENT: {
      if (o->getType()->isFloatingPointTy()) {
        llvm::Value *one = llvm::ConstantFP::get(o->getType(), 1);
        return llvm_builder->CreateFAdd(o, one, "finctmp");
      }
      else {
        llvm::Value *one = llvm::ConstantInt::get(o->getType(), 1);
        return llvm_builder->CreateAdd(o, one, "inctmp");
      }
    }
    case OpType::OP_DECREMENT: {
      if (o->getType()->isFloatingPointTy()) {
        llvm::Value *one = llvm::ConstantFP::get(o->getType(), 1);
        return llvm_builder->CreateFSub(o, one, "fdectmp");
      }
      else {
        llvm::Value *one = llvm::ConstantInt::get(o->getType(), 1);
        return llvm_builder->CreateSub(o, one, "dectmp");
      }
    }
    case OpType::OP_POST_INCREMENT: {
      if (o->getType()->isFloatingPointTy()) {
        llvm::Value *oldVal = llvm_builder->CreateLoad(o->getType(), o, "old");
        llvm::Value *one = llvm::ConstantFP::get(o->getType(), 1);
        llvm::Value *newVal = llvm_builder->CreateFAdd(oldVal, one, "new");
        llvm_builder->CreateStore(newVal, o);
        return oldVal;
      }
      else {
        llvm::Value *oldVal = llvm_builder->CreateLoad(o->getType(), o, "old");
        llvm::Value *one = llvm::ConstantInt::get(o->getType(), 1);
        llvm::Value *newVal = llvm_builder->CreateAdd(oldVal, one, "new");
        llvm_builder->CreateStore(newVal, o);
        return oldVal;
      }
    }
    case OpType::OP_POST_DECREMENT: {
      if (o->getType()->isFloatingPointTy()) {
        llvm::Value *oldVal = llvm_builder->CreateLoad(o->getType(), o, "old");
        llvm::Value *one = llvm::ConstantFP::get(o->getType(), 1);
        llvm::Value *newVal = llvm_builder->CreateFSub(oldVal, one, "new");
        llvm_builder->CreateStore(newVal, o);
        return oldVal;
      }
      else {
        llvm::Value *oldVal = llvm_builder->CreateLoad(o->getType(), o, "old");
        llvm::Value *one = llvm::ConstantInt::get(o->getType(), 1);
        llvm::Value *newVal = llvm_builder->CreateSub(oldVal, one, "new");
        llvm_builder->CreateStore(newVal, o);
        return oldVal;
      }
    }
    case OpType::OP_LOGIC_NOT: {
      if (o->getType()->isFloatingPointTy()) {
        llvm::Value *zero = llvm::ConstantFP::get(o->getType(), 0.0);
        return llvm_builder->CreateFCmpOEQ(o, zero);
      }
      else {
        llvm::Value *zero = llvm::ConstantInt::get(o->getType(), 0);
        return llvm_builder->CreateICmpEQ(o, zero);
      }
    }
    case OpType::OP_BITWISE_NOT: {
      // can't bitwise not on float
      llvm::Value *all_ones = llvm::ConstantInt::get(o->getType(), -1);
      return llvm_builder->CreateXor(o, all_ones);
    }
    case OpType::OP_ADDRESS_OF: {
      break;
    }
    case OpType::OP_INDIRECTION: {
      // TODO test this
      llvm::Type *value_ty = o->getType()->getPointerTo();
      return llvm_builder->CreateLoad(value_ty, o, "deref");
    }
    case OpType::OP_SIZEOF: {
      // TODO how to implement this
      fatal_error("Address of Not Yet Implemented");
    }
    case OpType::OP_CAST: {
      llvm::Type *from_type = o->getType();
      llvm::Type *to_type = map_to_LLVM_type(m_data_type);
      if (from_type->isIntegerTy()) {
        // Int -> Int
        if (to_type->isIntegerTy()) {
          if (from_type->getIntegerBitWidth() < to_type->getIntegerBitWidth()) {
            return m_data_type->m_is_signed ? llvm_builder->CreateSExt(o, to_type)
                                            : llvm_builder->CreateZExt(o, to_type);
          }
          else if (from_type->getIntegerBitWidth() > to_type->getIntegerBitWidth()) {
            return llvm_builder->CreateTrunc(o, to_type);
          }
          return o;
        }  // END: Int -> Int
        // Int -> Float | Double
        else if (to_type->isFloatingPointTy()) {
          // Need to extend
          bool is_signed = m_operand->m_data_type->m_is_signed;
          if (from_type->getIntegerBitWidth() < 4) {
            is_signed = true;
            llvm::Type *i32 = llvm::Type::getInt32Ty(*llvm_context);
            o = m_operand->m_data_type->m_is_signed ? llvm_builder->CreateSExt(o, i32, "exttmp")
                                                    : llvm_builder->CreateZExt(o, i32, "exttmp");
          }
          llvm::Instruction::CastOps inst = is_signed ? llvm::Instruction::SIToFP : llvm::Instruction::UIToFP;
          return llvm_builder->CreateCast(inst, o, to_type);
        }  // END: Int -> Float | Double
        // Int -> Pointer
        else if (to_type->isPointerTy()) {
          return llvm_builder->CreateCast(llvm::Instruction::IntToPtr, o, to_type);
        }
        return o;
      }  // END: Int
      // Float | Double
      else if (from_type->isFloatingPointTy()) {
        // Float | Double -> Int
        if (to_type->isIntegerTy()) {
          bool is_signed = m_operand->m_data_type->m_is_signed;
          return llvm_builder->CreateCast(is_signed ? llvm::Instruction::FPToSI : llvm::Instruction::FPToUI, o,
                                          to_type);
        }
        // Float -> Double
        else if (from_type->isFloatTy() && to_type->isDoubleTy()) {
          return llvm_builder->CreateFPExt(o, to_type);
        }
        // Double -> Float
        else if (from_type->isDoubleTy() && to_type->isFloatTy()) {
          return llvm_builder->CreateFPTrunc(o, to_type);
        }
        return o;
      }
      // Pointer
      else if (from_type->isPointerTy()) {
        if (to_type->isPointerTy()) return o;
        if (to_type->isIntegerTy()) {
          return llvm_builder->CreatePtrToInt(o, to_type);
        }
      }
    }
  }
  log_error("Invalid Unary Operation");
  return nullptr;
}

llvm::Value *gen_logical(ExpressionNode *lhs, ExpressionNode *rhs, bool is_and) {
  llvm::BasicBlock *first_block = llvm_builder->GetInsertBlock();
  llvm::Function *f = first_block->getParent();

  llvm::BasicBlock *next_block = llvm::BasicBlock::Create(*llvm_context, "next", f);
  llvm::BasicBlock *merge_block = llvm::BasicBlock::Create(*llvm_context, "merge", f);

  llvm::Value *l = lhs->codegen();

  if (l->getType()->isFloatingPointTy()) {
    l = llvm_builder->CreateFCmpUNE(l, llvm::Constant::getNullValue(l->getType()));
  }
  else {
    l = llvm_builder->CreateICmpNE(l, llvm::Constant::getNullValue(l->getType()));
  }

  if (is_and) {
    llvm_builder->CreateCondBr(l, next_block, merge_block);
  }
  else {
    llvm_builder->CreateCondBr(l, merge_block, next_block);
  }

  llvm_builder->SetInsertPoint(next_block);

  llvm::Value *r = rhs->codegen();

  if (r->getType()->isFloatingPointTy()) {
    r = llvm_builder->CreateFCmpUNE(r, llvm::Constant::getNullValue(r->getType()));
  }
  else {
    r = llvm_builder->CreateICmpNE(r, llvm::Constant::getNullValue(r->getType()));
  }
  llvm_builder->CreateBr(merge_block);

  llvm_builder->SetInsertPoint(merge_block);

  llvm::PHINode *phi =
      llvm_builder->CreatePHI(llvm::Type::getInt1Ty(*llvm_context), 2, is_and ? "and_result" : "or_result");
  phi->addIncoming(l, first_block);
  phi->addIncoming(r, next_block);

  return phi;
}

llvm::Value *BinaryOpNode::codegen() {
  m_left_operand->m_is_lvalue = m_operation == OpType::OP_ASSIGN;

  if (m_operation == OpType::OP_LOGIC_AND || m_operation == OpType::OP_LOGIC_OR) {
    return gen_logical(m_left_operand.get(), m_right_operand.get(), m_operation == OpType::OP_LOGIC_AND);
  }

  llvm::Value *l = m_left_operand->codegen();
  m_right_operand->m_is_lvalue = false;
  llvm::Value *r = m_right_operand->codegen();

  if (!l || !r) return nullptr;

  llvm::Type *c_type = map_to_LLVM_type(m_data_type);
  bool is_signed = m_data_type->m_is_signed;

  switch (m_operation) {
    case OpType::OP_ADDITION: {
      if (c_type->isFloatingPointTy()) {
        return llvm_builder->CreateFAdd(l, r);
      }
      if (c_type->isPointerTy()) {
        PointerType *p_type = static_cast<PointerType *>(m_data_type.get());
        return llvm_builder->CreateGEP(map_to_LLVM_type(p_type->m_base_type), l, {r});
      }
      return llvm_builder->CreateAdd(l, r);
      break;
    }
    case OpType::OP_SUBTRACTION: {
      if (c_type->isFloatingPointTy()) {
        return llvm_builder->CreateFSub(l, r);
      }
      if (c_type->isPointerTy()) {
        r = llvm_builder->CreateSub(llvm::Constant::getNullValue(r->getType()), r);
        PointerType *p_type = static_cast<PointerType *>(m_data_type.get());
        return llvm_builder->CreateGEP(map_to_LLVM_type(p_type->m_base_type), l, {r});
      }
      if (c_type->isIntegerTy()) {
        return llvm_builder->CreateSub(l, r);
      }
      break;
    }
    case OpType::OP_MULTIPLICATION: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFMul(l, r);
      }
      if (c_type->isIntegerTy()) {
        return llvm_builder->CreateMul(l, r);
      }
      break;
    }
    case OpType::OP_DIVISION: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFDiv(l, r);
      }
      if (c_type->isIntegerTy()) {
        return is_signed ? llvm_builder->CreateSDiv(l, r) : llvm_builder->CreateUDiv(l, r);
      }
      break;
    }
    case OpType::OP_MODULO: {
      return is_signed ? llvm_builder->CreateSRem(l, r) : llvm_builder->CreateURem(l, r);
    }
    case OpType::OP_BIT_RIGHT: {
      return is_signed ? llvm_builder->CreateAShr(l, r) : llvm_builder->CreateLShr(l, r);
    }
    case OpType::OP_BIT_LEFT: {
      return is_signed ? llvm_builder->CreateAShr(l, r) : llvm_builder->CreateLShr(l, r);
    }
    case OpType::OP_BIT_AND: {
      return llvm_builder->CreateAnd(l, r);
    }
    case OpType::OP_BIT_OR: {
      return llvm_builder->CreateOr(l, r);
    }
    case OpType::OP_BIT_XOR: {
      return llvm_builder->CreateXor(l, r);
    }
    case OpType::OP_COMP_GREATER: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFCmpUGT(l, r);
      }
      return is_signed ? llvm_builder->CreateICmpSGT(l, r) : llvm_builder->CreateICmpUGT(l, r);
    }
    case OpType::OP_COMP_LESS: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFCmpULT(l, r);
      }
      return is_signed ? llvm_builder->CreateICmpSLT(l, r) : llvm_builder->CreateICmpULT(l, r);
    }
    case OpType::OP_COMP_GREATER_EQUAL: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFCmpUGE(l, r);
      }
      return is_signed ? llvm_builder->CreateICmpSGE(l, r) : llvm_builder->CreateICmpUGE(l, r);
    }
    case OpType::OP_COMP_LESS_EQUAL: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFCmpULE(l, r);
      }
      return is_signed ? llvm_builder->CreateICmpSLE(l, r) : llvm_builder->CreateICmpULE(l, r);
    }
    case OpType::OP_COMP_EQUAL: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFCmpUEQ(l, r);
      }
      return llvm_builder->CreateICmpEQ(l, r);
    }
    case OpType::OP_COMP_NOT_EQUAL: {
      if (c_type->isFloatTy()) {
        return llvm_builder->CreateFCmpUNE(l, r);
      }
      return llvm_builder->CreateICmpNE(l, r);
    }
    case OpType::OP_LOGIC_AND: {
      break;
    }
    case OpType::OP_LOGIC_OR: {
      break;
    }
    case OpType::OP_ASSIGN: {
      llvm_builder->CreateStore(r, l);
      m_left_operand->m_is_lvalue = false;
      return m_left_operand->codegen();
    }
    case OpType::OP_ARRAY_SUBSCRIPT: {
      // TODO array type?
      PointerType *ptr_type = static_cast<PointerType *>(m_left_operand->m_data_type.get());
      llvm::Type *base_type = map_to_LLVM_type(ptr_type->m_base_type);
      l = llvm_builder->CreateGEP(base_type, l, {r});
      return llvm_builder->CreateLoad(base_type, l);
    }
    case OpType::OP_COMMA: {
      return r;
    }
  }

  fatal_error("Unexpected Binary Operation");
}

llvm::Value *TernaryOpNode::codegen() {
  llvm::Value *cond = m_condition->codegen();
  llvm::Value *t = m_true_expr->codegen();
  llvm::Value *f = m_false_expr->codegen();

  if (!cond | !t | !f) return nullptr;

  return llvm_builder->CreateSelect(cond, t, f);
}

llvm::Value *MemberAccessNode::codegen() {
  // TODO
  fatal_error("Member Access Codegen not yet implemented");
}

llvm::Value *CallNode::codegen() {
  std::vector<llvm::Value *> args;
  args.reserve(m_args.size());

  for (auto &arg : m_args) {
    llvm::Value *arg_val = arg->codegen();
    if (!arg_val) return nullptr;
    args.push_back(arg_val);
  }

  if (!m_name.empty()) {
    llvm::Function *callee_func = llvm_module->getFunction(m_name);

    // TODO Function Prototypes
    if (callee_func == nullptr) return nullptr;

    return llvm_builder->CreateCall(callee_func, args, "calltmp");
  }

  llvm::Value *callee = m_callee->codegen();

  if (!callee) return nullptr;

  // Can static cast callee data_type since it should be functionType
  FunctionType *func_type = static_cast<FunctionType *>(m_callee->m_data_type.get());
  llvm::Type *return_ty = map_to_LLVM_type(m_data_type);

  std::vector<llvm::Type *> param_tys;
  param_tys.reserve(m_args.size());
  for (auto &param_data_type : func_type->m_param_types) {
    param_tys.push_back((map_to_LLVM_type(param_data_type)));
  }

  llvm::FunctionType *function_ty = llvm::FunctionType::get(return_ty, param_tys, false);
  llvm::Type *expected_ty = function_ty->getPointerTo();
  if (callee->getType() != expected_ty) {
    callee = llvm_builder->CreateBitCast(callee, expected_ty, "fncast");
  }

  return llvm_builder->CreateCall(function_ty, callee, args, "calltmp");
}

llvm::Value *IdentifierNode::codegen() {
  llvm::Value *ptr = named_values[m_name];
  if (!ptr) {
    fatal_error(std::format(R"(Identifier "{}" not definined in LLVM Context)", m_name));
  }
  if (m_is_lvalue) {
    return ptr;
  }
  return llvm_builder->CreateLoad(map_to_LLVM_type(m_data_type), ptr, m_name);
}

llvm::Value *ConstantNode::codegen() {
  llvm::Type *l_type = map_to_LLVM_type(m_data_type);
  switch (m_val_type) {
    case ValType::SIGNED_CHAR: {
      return llvm::ConstantInt::get(l_type, get_val<int8_t>(), true);
    }
    case ValType::UNSIGNED_CHAR: {
      return llvm::ConstantInt::get(l_type, get_val<uint8_t>(), false);
    }
    case ValType::SIGNED_SHORT: {
      return llvm::ConstantInt::get(l_type, get_val<int16_t>(), true);
    }
    case ValType::UNSIGNED_SHORT: {
      return llvm::ConstantInt::get(l_type, get_val<uint16_t>(), false);
    }
    case ValType::SIGNED_INTEGER: {
      return llvm::ConstantInt::get(l_type, get_val<int32_t>(), true);
    }
    case ValType::UNSIGNED_INTEGER: {
      return llvm::ConstantInt::get(l_type, get_val<uint32_t>(), false);
    }
    case ValType::SIGNED_LONG: {
      return llvm::ConstantInt::get(l_type, get_val<int64_t>(), true);
    }
    case ValType::UNSIGNED_LONG: {
      return llvm::ConstantInt::get(l_type, get_val<uint64_t>(), false);
    }
    case ValType::FLOAT: {
      return llvm::ConstantFP::get(*llvm_context, llvm::APFloat(get_val<float>()));
    }
    case ValType::DOUBLE: {
      return llvm::ConstantFP::get(*llvm_context, llvm::APFloat(get_val<double>()));
    }
    case ValType::STRING: {
      return llvm::ConstantDataArray::getString(*llvm_context, get_val<std::string>(), true);
    }
  }
  fatal_error("Unknown Constant type");
  return nullptr;
}

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
// Statements

void LabelStmt::codegen() {
  llvm::BasicBlock *parent_block = llvm_builder->GetInsertBlock();
  llvm::BasicBlock *label_block = llvm::BasicBlock::Create(*llvm_context, m_label, parent_block->getParent());

  if (!parent_block->getTerminator()) {
    llvm_builder->CreateBr(label_block);
  }

  llvm_builder->SetInsertPoint(label_block);
  labeled_blocks[m_label] = label_block;
}

void CaseStmt::codegen() {
  // Case Statement code gen handled in SwitchStmt
  return;
}

void DefaultStmt::codegen() {
  // Default Statement code gen handled in SwitchStmt
  return;
}

void CompoundStmt::codegen() {
  for (auto &n : m_stmts) {
    if (!codegen_node(n.get())) return;
  }
}

void ExpressionStmt::codegen() { m_expr->codegen(); }

void IfStmt::codegen() {
  llvm::Value *cond = m_condition->codegen();
  if (!cond) return;

  if (cond->getType()->isFloatTy() || cond->getType()->isDoubleTy()) {
    cond = llvm_builder->CreateFCmpUNE(cond, llvm::ConstantFP::get(*llvm_context, llvm::APFloat(0.0)), "ifcond");
  }
  else {
    cond = llvm_builder->CreateICmpNE(cond, llvm::Constant::getNullValue(cond->getType()));
  }

  llvm::BasicBlock *condition = llvm_builder->GetInsertBlock();
  llvm::Function *f = llvm_builder->GetInsertBlock()->getParent();

  llvm::BasicBlock *then_block = llvm::BasicBlock::Create(*llvm_context, "then", f);
  // llvm::BasicBlock *else_block = m_false_stmt ? llvm::BasicBlock::Create(*llvm_context, "else", f) : nullptr;
  // llvm::BasicBlock *merge_block = llvm::BasicBlock::Create(*llvm_context, "merge", f);

  // if (else_block) {
  //   llvm_builder->CreateCondBr(cond, then_block, else_block);
  // }
  // else {
  //   llvm_builder->CreateCondBr(cond, then_block, merge_block);
  // }

  llvm_builder->SetInsertPoint(then_block);
  m_true_stmt->codegen();

  llvm::BasicBlock *else_block = nullptr;
  llvm::BasicBlock *merge_block = nullptr;
  if (m_false_stmt) {
    else_block = llvm::BasicBlock::Create(*llvm_context, "else", f);
    llvm_builder->SetInsertPoint(else_block);
    m_false_stmt->codegen();

    if (!else_block->getTerminator()) {
      merge_block = llvm::BasicBlock::Create(*llvm_context, "merge", f);
      llvm_builder->CreateBr(merge_block);
    }

    llvm_builder->SetInsertPoint(condition);
    llvm_builder->CreateCondBr(cond, then_block, else_block);
  }
  else {
    merge_block = llvm::BasicBlock::Create(*llvm_context, "merge", f);
    llvm_builder->CreateCondBr(cond, then_block, merge_block);
  }

  if (!then_block->getTerminator()) {
    llvm_builder->SetInsertPoint(then_block);
    llvm_builder->CreateBr(merge_block);
  }

  if (merge_block) {
    llvm_builder->SetInsertPoint(merge_block);
  }
}

void SwitchStmt::codegen() {
  std::vector<std::pair<CaseStmt *, std::vector<Node *>>> cases;
  std::pair<DefaultStmt *, std::vector<Node *>> deft = std::make_pair<DefaultStmt *, std::vector<Node *>>(nullptr, {});
  CompoundStmt *c_stmt = dynamic_cast<CompoundStmt *>(m_stmt.get());

  if (!c_stmt) return;

  for (size_t i = 0; i < c_stmt->m_stmts.size();) {
    auto &stmt = c_stmt->m_stmts[i++];
    if (stmt->m_node_type == NodeType::CASE_STATEMENT) {
      std::pair<CaseStmt *, std::vector<Node *>> case_stmt =
          std::make_pair<CaseStmt *, std::vector<Node *>>(static_cast<CaseStmt *>(stmt.get()), {});

      while (i < c_stmt->m_stmts.size() && c_stmt->m_stmts[i]->m_node_type != NodeType::CASE_STATEMENT &&
             c_stmt->m_stmts[i]->m_node_type != NodeType::DEFAULT_STATEMENT) {
        case_stmt.second.push_back(c_stmt->m_stmts[i++].get());
      }
    }
    else if (stmt->m_node_type == NodeType::DEFAULT_STATEMENT) {
      deft.first = static_cast<DefaultStmt *>(stmt.get());

      while (i < c_stmt->m_stmts.size() && c_stmt->m_stmts[i]->m_node_type != NodeType::CASE_STATEMENT) {
        deft.second.push_back(c_stmt->m_stmts[i++].get());
      }
    }
  }

  llvm::Value *expr = m_expr->codegen();

  llvm::Function *f = llvm_builder->GetInsertBlock()->getParent();

  llvm::SwitchInst *switch_inst = llvm_builder->CreateSwitch(expr, nullptr, cases.size());
  llvm::BasicBlock *merge_block = llvm::BasicBlock::Create(*llvm_context, "switch_end", f);

  llvm::BasicBlock *prev_break_dest = break_dest;
  break_dest = merge_block;

  for (auto &case_pair : cases) {
    llvm::BasicBlock *prev_case = llvm_builder->GetInsertBlock();
    llvm::BasicBlock *case_block = llvm::BasicBlock::Create(*llvm_context, "", f);
    if (!prev_case->getTerminator()) {
      llvm_builder->CreateBr(case_block);
    }

    llvm::ConstantInt *case_expr = llvm::dyn_cast<llvm::ConstantInt>(case_pair.first->m_expr->codegen());
    if (!case_expr) {
      // TODO error
      return;
    }

    switch_inst->addCase(case_expr, case_block);
    llvm_builder->SetInsertPoint(case_block);
    for (auto n : case_pair.second) {
      if (!codegen_node(n)) return;
    }
  }

  if (deft.first) {
    llvm::BasicBlock *prev_case = llvm_builder->GetInsertBlock();
    llvm::BasicBlock *default_block = llvm::BasicBlock::Create(*llvm_context, "default", f);
    if (!prev_case->getTerminator()) {
      llvm_builder->CreateBr(default_block);
    }

    switch_inst->setDefaultDest(default_block);
    llvm_builder->SetInsertPoint(default_block);
    for (auto n : deft.second) {
      if (!codegen_node(n)) return;
    }
  }
  else {
    switch_inst->setDefaultDest(merge_block);
  }

  llvm::BasicBlock *prev_block = llvm_builder->GetInsertBlock();
  if (!prev_block->getTerminator()) {
    llvm_builder->CreateBr(merge_block);
  }
  break_dest = prev_break_dest;

  llvm_builder->SetInsertPoint(merge_block);
}

void WhileStmt::codegen() {
  llvm::BasicBlock *prev_block = llvm_builder->GetInsertBlock();
  llvm::Function *f = prev_block->getParent();

  llvm::BasicBlock *condition_check = llvm::BasicBlock::Create(*llvm_context, "while_condition", f);
  llvm::BasicBlock *body = llvm::BasicBlock::Create(*llvm_context, "body", f);
  llvm::BasicBlock *done = llvm::BasicBlock::Create(*llvm_context, "done", f);

  if (!prev_block->getTerminator()) {
    if (m_while_type == WhileType::WHILE) {
      llvm_builder->CreateBr(condition_check);
    }
    else {
      llvm_builder->CreateBr(body);
    }
  }

  llvm_builder->SetInsertPoint(condition_check);
  llvm::Value *cond = m_condition->codegen();
  if (cond->getType()->isFloatingPointTy()) {
    cond = llvm_builder->CreateFCmpUNE(cond, llvm::Constant::getNullValue(cond->getType()));
  }
  else {
    cond = llvm_builder->CreateICmpNE(cond, llvm::Constant::getNullValue(cond->getType()));
  }

  llvm_builder->CreateCondBr(cond, body, done);

  llvm::BasicBlock *prev_break_dest = break_dest;
  llvm::BasicBlock *prev_continue_dest = continue_dest;
  break_dest = done;
  continue_dest = condition_check;

  llvm_builder->SetInsertPoint(body);
  m_stmt->codegen();

  break_dest = prev_break_dest;
  continue_dest = prev_continue_dest;

  prev_block = llvm_builder->GetInsertBlock();
  if (!prev_block->getTerminator()) {
    llvm_builder->CreateBr(condition_check);
  }

  llvm_builder->SetInsertPoint(done);
}

void ForStmt::codegen() {
  if (m_init_clause) {
    if (!codegen_node(m_init_clause.get())) return;
  }

  llvm::BasicBlock *previous_block = llvm_builder->GetInsertBlock();
  llvm::Function *f = previous_block->getParent();
  llvm::BasicBlock *condition = llvm::BasicBlock::Create(*llvm_context, "for_condition", f);
  llvm::BasicBlock *iterator = llvm::BasicBlock::Create(*llvm_context, "for_iterator", f);
  llvm::BasicBlock *body = llvm::BasicBlock::Create(*llvm_context, "for_body", f);
  llvm::BasicBlock *end = llvm::BasicBlock::Create(*llvm_context, "for_end", f);

  if (!previous_block->getTerminator()) {
    llvm_builder->CreateBr(condition);
  }

  llvm_builder->SetInsertPoint(condition);
  llvm::Value *cond = m_condition->codegen();
  if (cond->getType()->isFloatingPointTy()) {
    cond = llvm_builder->CreateFCmpUNE(cond, llvm::Constant::getNullValue(cond->getType()), "cmptmp");
  }
  else {
    cond = llvm_builder->CreateICmpNE(cond, llvm::Constant::getNullValue(cond->getType()), "cmptmp");
  }

  llvm_builder->CreateCondBr(cond, body, end);

  llvm::BasicBlock *prev_break_dest = break_dest;
  llvm::BasicBlock *prev_continue_dest = continue_dest;
  break_dest = end;
  continue_dest = condition;

  llvm_builder->SetInsertPoint(body);
  m_stmt->codegen();

  break_dest = prev_break_dest;
  continue_dest = prev_continue_dest;

  previous_block = llvm_builder->GetInsertBlock();
  if (!previous_block->getTerminator()) {
    llvm_builder->CreateBr(iterator);
  }

  llvm_builder->SetInsertPoint(iterator);
  if (m_iteration) {
    m_iteration->codegen();
  }
  llvm_builder->CreateBr(condition);

  llvm_builder->SetInsertPoint(end);
}

void ControlStmt::codegen() {
  if (m_control == ControlType::BREAK) {
    if (break_dest == nullptr) fatal_error("No break destination specified");
    llvm_builder->CreateBr(break_dest);
  }
  else {
    if (continue_dest == nullptr) fatal_error("No break destination specified");
    llvm_builder->CreateBr(continue_dest);
  }
}

void ReturnStmt::codegen() {
  if (m_return_value) {
    llvm::Value *ret_val = m_return_value->codegen();
    llvm_builder->CreateRet(ret_val);
  }
  else {
    llvm_builder->CreateRetVoid();
  }
}

void GotoStmt::codegen() { llvm_builder->CreateBr(labeled_blocks[m_label]); }

/////////////////////////////////////////////////////////////////////

// TODO global variables
llvm::Value *VariableDeclaration::codegen() {
  llvm::Function *func = llvm_builder->GetInsertBlock()->getParent();
  llvm::AllocaInst *alloc = alloc_variable(func, map_to_LLVM_type(m_data_type), nullptr, m_identifier);
  named_values[m_identifier] = alloc;

  if (m_initializer) {
    llvm::Value *init_val = m_initializer->codegen();
    llvm_builder->CreateStore(init_val, alloc);
  }

  return alloc;
}

// TODO global variables
llvm::Value *ArrayDeclaration::codegen() {
  llvm::Function *func = llvm_builder->GetInsertBlock()->getParent();

  // TODO
  if (!m_size) {
    fatal_error("Array VLA not yet implemented");
  }

  llvm::Value *size = m_size->codegen();
  llvm::AllocaInst *alloc = alloc_variable(func, map_to_LLVM_type(m_data_type), size, m_identifier);
  named_values[m_identifier] = alloc;

  // TODO variable length arrays
  if (m_initializer) {
    llvm::Value *init_val = m_initializer->codegen();
    llvm_builder->CreateStore(init_val, alloc);
  }

  return alloc;
}

llvm::Value *FunctionDefinition::codegen() {
  llvm::Function *f = llvm_module->getFunction(m_identifier);

  if (!f) {
    std::vector<llvm::Type *> params;
    params.reserve(m_params.size());

    FunctionType *func_data_type = static_cast<FunctionType *>(m_data_type.get());
    for (auto &param : func_data_type->m_param_types) {
      params.push_back(map_to_LLVM_type(param));
    }

    llvm::Type *ret_ty = map_to_LLVM_type(func_data_type->m_return_type);

    llvm::FunctionType *func_ty = llvm::FunctionType::get(ret_ty, params, false);

    f = llvm::Function::Create(func_ty, llvm::Function::ExternalLinkage, m_identifier, *llvm_module);

    size_t i = 0;
    for (auto &arg : f->args()) {
      arg.setName(m_params[i++]->m_identifier);
    }
  }

  if (!f) return nullptr;
  if (m_body == nullptr) return f;

  llvm::BasicBlock *block = llvm::BasicBlock::Create(*llvm_context, "entry", f);
  llvm_builder->SetInsertPoint(block);

  auto arg_iter = f->arg_begin();

  for (auto &param : m_params) {
    llvm::Value *alloc = param->codegen();
    llvm_builder->CreateStore(&*arg_iter, alloc);
    ++arg_iter;
  }

  m_body->codegen();

  if (llvm::verifyFunction(*f)) {
    log_error("LLVM Error Occurred... and ignored");
  }

  llvm_fpm->run(*f, *llvm_fam);

  return f;
}

std::vector<llvm::Value *> TranslationUnit::codegen() {
  llvm_context = std::make_unique<llvm::LLVMContext>();
  llvm_builder = std::make_unique<llvm::IRBuilder<>>(*llvm_context);
  llvm_module = std::make_unique<llvm::Module>(m_filename, *llvm_context);

  // Optimization Passes
  llvm_fpm = std::make_unique<llvm::FunctionPassManager>();
  llvm_lam = std::make_unique<llvm::LoopAnalysisManager>();
  llvm_fam = std::make_unique<llvm::FunctionAnalysisManager>();
  llvm_cgam = std::make_unique<llvm::CGSCCAnalysisManager>();
  llvm_mam = std::make_unique<llvm::ModuleAnalysisManager>();
  llvm_pic = std::make_unique<llvm::PassInstrumentationCallbacks>();
  llvm_si = std::make_unique<llvm::StandardInstrumentations>(*llvm_context, true);

  llvm_si->registerCallbacks(*llvm_pic, llvm_mam.get());

  llvm_fpm->addPass(llvm::InstCombinePass());
  llvm_fpm->addPass(llvm::ReassociatePass());
  llvm_fpm->addPass(llvm::GVNPass());
  llvm_fpm->addPass(llvm::SimplifyCFGPass());

  llvm::PassBuilder llvm_pb;
  llvm_pb.registerModuleAnalyses(*llvm_mam);
  llvm_pb.registerFunctionAnalyses(*llvm_fam);
  llvm_pb.crossRegisterProxies(*llvm_lam, *llvm_fam, *llvm_cgam, *llvm_mam);

  std::vector<llvm::Value *> result;
  for (auto &decl : m_program) {
    if (llvm::Value *v = decl->codegen()) {
      result.push_back(v);
    }
  }
  result.back()->dump();
  return result;
}

}  // namespace JCC
#pragma once

#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/IR/InstrTypes.h>
#include <llvm-18/llvm/IR/Instructions.h>
#include <llvm-18/llvm/IR/Value.h>

#include <cstdint>
#include <format>
#include <memory>
#include <ostream>
#include <string>
#include <unordered_map>
#include <variant>

#include "llvm/IR/InstVisitor.h"

namespace JCC {

namespace LC2K {

template <class... Ts>
struct overloads : Ts... {
  using Ts::operator()...;
};

struct Instruction;
using UniqueInst = std::unique_ptr<Instruction>;

struct Instruction {
  std::string label = "";
  std::string comment = "";

  virtual std::string emit() = 0;

  static UniqueInst CreateAdd(int a, int b, int dest, std::string label = "", std::string comment = "");
  static UniqueInst CreateNor(int a, int b, int dest, std::string label = "", std::string comment = "");

  static UniqueInst CreateLw(int a, int b, std::variant<std::string, int16_t> addr, std::string label = "",
                             std::string comment = "");
  static UniqueInst CreateSw(int a, int b, std::variant<std::string, int16_t> addr, std::string label = "",
                             std::string comment = "");
  static UniqueInst CreateBeq(int a, int b, std::variant<std::string, int16_t> addr, std::string label = "",
                              std::string comment = "");

  static UniqueInst CreateJalr(int a, int b, std::string label = "", std::string comment = "");

  static UniqueInst CreateHalt(std::string label = "", std::string comment = "");
  static UniqueInst CreateNoop(std::string label = "", std::string comment = "");

  static UniqueInst CreateData(std::variant<std::string, int32_t> data, std::string label = "",
                               std::string comment = "");
};

enum class Reg { R0, R1, R2, R3, R4, R5, R6, R7 };

struct RType : public Instruction {
  enum class Opcode { ADD, NOR };
  Opcode op;
  Reg a;
  Reg b;
  Reg dest;

  std::string emit() override {
    std::string op_str = op == Opcode::ADD ? "add" : "nor";
    return std::format("{}\t{} {} {} {}\t; {}\n", label, op_str, static_cast<uint8_t>(a), static_cast<uint8_t>(b),
                       static_cast<uint8_t>(dest), comment);
  }

  RType(Opcode op, Reg a, Reg b, Reg dest) : op(op), a(a), b(b), dest(dest) {}
};

struct IType : public Instruction {
  enum class Opcode { LW, SW, BEQ };
  Opcode op;
  Reg a;
  Reg b;
  std::variant<std::string, int16_t> address;

  std::string emit() override {
    std::string op_str = op == Opcode::LW ? "lw" : op == Opcode::SW ? "sw" : "beq";

    const auto visitor =
        overloads{[](std::string addr) { return addr; }, [](int16_t addr) { return std::to_string(addr); }};

    std::string addr_str = std::visit(visitor, address);

    return std::format("{}\t{} {} {} {}\t; {}\n", label, op_str, static_cast<uint8_t>(a), static_cast<uint8_t>(b),
                       addr_str, comment);
  }

  IType(Opcode op, Reg a, Reg b, std::variant<std::string, int16_t> address) : op(op), a(a), b(b), address(address) {}
};

struct JType : public Instruction {
  // Always Jalr
  Reg a;
  Reg b;

  std::string emit() override {
    return std::format("{}\tjalr {} {}\t; {}\n", label, static_cast<uint8_t>(a), static_cast<uint8_t>(b), comment);
  }

  JType(Reg a, Reg b) : a(a), b(b) {}
};

struct OType : public Instruction {
  enum class Opcode { HALT, NOOP };
  Opcode op;

  std::string emit() override {
    std::string op_str = op == Opcode::HALT ? "halt" : "noop";
    return std::format("{}\t{}\t; {}\n", label, op_str, comment);
  }

  OType(Opcode op) : op(op) {}
};

struct Data : public Instruction {
  std::variant<std::string, int32_t> data;

  std::string emit() override {
    const auto visitor =
        overloads{[](std::string addr) { return addr; }, [](int32_t addr) { return std::to_string(addr); }};

    std::string data_str = std::visit(visitor, data);

    if (!data_str.empty()) {
      data_str = ".fill " + data_str;
    }

    return std::format("{}\t{}\t; {}\n", label, data_str, comment);
  }

  Data(std::variant<std::string, int32_t> data) : data(data) {}
};

struct Emitter : public llvm::InstVisitor<Emitter> {
  std::unordered_map<int32_t, std::string> constants;

  llvm::ConstantInt *one;
  llvm::ConstantInt *neg_one;

  struct LoweredBlock {
    std::string label;
    std::vector<UniqueInst> text;
    std::vector<std::string> successors;
    std::vector<std::string> predecessors;
    std::vector<std::pair<int, int>> phi_nodes;

    LoweredBlock(std::string label) : label(label) {}
  };

  struct LoweredFunc {
    std::string name;
    int stack_size = 0;
    int reg = 8;
    std::vector<std::unique_ptr<LoweredBlock>> blocks;
    std::unordered_map<llvm::Value *, int> virtual_registers;
    std::unordered_map<llvm::Value *, int> alloca_offsets;

    LoweredFunc(std::string name) : name(name) {}
  };

  LoweredFunc *cur_func;
  LoweredBlock *cur_block;
  std::vector<std::unique_ptr<LoweredFunc>> funcs;

  // Creates and sets current function to inputted function
  void CreateFunc(std::string name) {
    cur_func = new LoweredFunc(name);
    funcs.push_back(std::unique_ptr<LoweredFunc>(cur_func));
  }
  LoweredBlock *CreateBlock(std::string label) {
    LoweredBlock *block = new LoweredBlock(label);
    funcs.back()->blocks.push_back(std::unique_ptr<LoweredBlock>(block));
    return block;
  }

  std::vector<UniqueInst> data;
  std::unordered_map<llvm::Function *, std::string> func_addrs;

  Emitter();

  std::string mangle(std::string label, bool shorten = false);

  int get_virt_reg(llvm::Value *val);

  inline void CreateAdd(int a, int b, int dest, std::string label = "", std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateAdd(a, b, dest, label, comment));
  }
  inline void CreateNor(int a, int b, int dest, std::string label = "", std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateNor(a, b, dest, label, comment));
  }
  inline void CreateLw(int a, int b, std::variant<std::string, int16_t> addr, std::string label = "",
                       std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateLw(a, b, addr, label, comment));
  }
  inline void CreateSw(int a, int b, std::variant<std::string, int16_t> addr, std::string label = "",
                       std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateSw(a, b, addr, label, comment));
  }
  inline void CreateBeq(int a, int b, std::variant<std::string, int16_t> addr, std::string label = "",
                        std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateBeq(a, b, addr, label, comment));
  }
  inline void CreateJalr(int a, int b, std::string label = "", std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateJalr(a, b, label, comment));
  }
  inline void CreateHalt(std::string label = "", std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateHalt(label, comment));
  }
  inline void CreateNoop(std::string label = "", std::string comment = "") {
    cur_block->text.push_back(Instruction::CreateNoop(label, comment));
  }
  inline void CreateData(std::variant<std::string, int32_t> p_data, std::string label = "", std::string comment = "") {
    data.push_back(Instruction::CreateData(p_data, label, comment));
  }

  // Takes in a condition, returns virtual register of result
  int compareEQ(llvm::Value *v1, llvm::Value *v2);

  void visitAdd(llvm::BinaryOperator &I);
  void visitSub(llvm::BinaryOperator &I);

  void visitMul(llvm::BinaryOperator &I);
  void visitUDiv(llvm::BinaryOperator &I);
  void visitSDiv(llvm::BinaryOperator &I);

  void visitAnd(llvm::BinaryOperator &I);
  void visitOr(llvm::BinaryOperator &I);
  void visitXor(llvm::BinaryOperator &I);

  void visitShl(llvm::BinaryOperator &I);
  void visitShr(llvm::BinaryOperator &I);
  void visitAshr(llvm::BinaryOperator &I);

  void visitAllocaInst(llvm::AllocaInst &I);
  void visitLoadInst(llvm::LoadInst &I);
  void visitStoreInst(llvm::StoreInst &I);
  void visitGetElementPtrInst(llvm::GetElementPtrInst &I);

  std::string branchHelper(llvm::BranchInst &I, int succ_num);
  LoweredBlock *findOrCreateBlock(std::string name, bool set_pred = true);

  void visitBranchInst(llvm::BranchInst &I);
  void visitSwitchInst(llvm::SwitchInst &I);
  void visitReturnInst(llvm::ReturnInst &I);
  void visitPHINode(llvm::PHINode &I);

  void visitICmpInst(llvm::ICmpInst &I);

  void pushStack(llvm::Value *val);
  int popStack(bool collect = false);

  void visitCallInst(llvm::CallInst &I);

  void visitSelectInst(llvm::SelectInst &I);

  void visitCastInst(llvm::CastInst &I);
  void visitInstruction(llvm::Instruction &I);

  // void visitModule(llvm::Module &M);
  void visitFunction(llvm::Function &F);
  // void visitBasicBlock(llvm::BasicBlock &BB);

  void debug_emit(std::ostream &os);
};

}  // namespace LC2K

}  // namespace JCC
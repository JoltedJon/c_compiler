#include "lc2k.hpp"

#include <llvm-18/llvm/IR/BasicBlock.h>
#include <llvm-18/llvm/IR/Constant.h>
#include <llvm-18/llvm/IR/InstrTypes.h>
#include <llvm-18/llvm/IR/Instructions.h>
#include <llvm-18/llvm/IR/Value.h>
#include <llvm-18/llvm/Support/Casting.h>
#include <llvm-18/llvm/Support/raw_ostream.h>

#include <cmath>
#include <iostream>
#include <memory>

#include "../ansi_colors.hpp"
#include "../middleend/codegen.hpp"
#include "src/ast.hpp"

namespace JCC {
namespace LC2K {

#define fatal_error(p_error_message)                                                                                \
  do {                                                                                                              \
    std::cerr << ANSI_COLOR_RED << "Fatal Error - Please Report Bug: " << ANSI_COLOR_RESET << "\n\t"                \
              << __PRETTY_FUNCTION__ << ":" << __LINE__ << "\n\tlc2k generation: " << p_error_message << std::endl; \
    std::abort();                                                                                                   \
  } while (0)

UniqueInst Instruction::CreateAdd(int a, int b, int dest, std::string label, std::string comment) {
  UniqueInst inst =
      UniqueInst(new RType(RType::Opcode::ADD, static_cast<Reg>(a), static_cast<Reg>(b), static_cast<Reg>(dest)));
  inst->label = label;
  inst->comment = comment;
  return inst;
}
UniqueInst Instruction::CreateNor(int a, int b, int dest, std::string label, std::string comment) {
  UniqueInst inst =
      UniqueInst(new RType(RType::Opcode::NOR, static_cast<Reg>(a), static_cast<Reg>(b), static_cast<Reg>(dest)));
  inst->label = label;
  inst->comment = comment;
  return inst;
}

UniqueInst Instruction::CreateLw(int a, int b, std::variant<std::string, int16_t> addr, std::string label,
                                 std::string comment) {
  UniqueInst inst = UniqueInst(new IType(IType::Opcode::LW, static_cast<Reg>(a), static_cast<Reg>(b), addr));
  inst->label = label;
  inst->comment = comment;
  return inst;
}
UniqueInst Instruction::CreateSw(int a, int b, std::variant<std::string, int16_t> addr, std::string label,
                                 std::string comment) {
  UniqueInst inst = UniqueInst(new IType(IType::Opcode::SW, static_cast<Reg>(a), static_cast<Reg>(b), addr));
  inst->label = label;
  inst->comment = comment;
  return inst;
}
UniqueInst Instruction::CreateBeq(int a, int b, std::variant<std::string, int16_t> addr, std::string label,
                                  std::string comment) {
  UniqueInst inst = UniqueInst(new IType(IType::Opcode::BEQ, static_cast<Reg>(a), static_cast<Reg>(b), addr));
  inst->label = label;
  inst->comment = comment;
  return inst;
}

UniqueInst Instruction::CreateJalr(int a, int b, std::string label, std::string comment) {
  UniqueInst inst = UniqueInst(new JType(static_cast<Reg>(a), static_cast<Reg>(b)));
  inst->label = label;
  inst->comment = comment;
  return inst;
}

UniqueInst Instruction::CreateHalt(std::string label, std::string comment) {
  UniqueInst inst = UniqueInst(new OType(OType::Opcode::HALT));
  inst->label = label;
  inst->comment = comment;
  return inst;
}
UniqueInst Instruction::CreateNoop(std::string label, std::string comment) {
  UniqueInst inst = UniqueInst(new OType(OType::Opcode::NOOP));
  inst->label = label;
  inst->comment = comment;
  return inst;
}

UniqueInst Instruction::CreateData(std::variant<std::string, int32_t> data, std::string label, std::string comment) {
  UniqueInst inst = UniqueInst(new Data(data));
  inst->label = label;
  inst->comment = comment;
  return inst;
}

/*
Register Calling Convention:
r0  - Always Zero
r1  - First Function Parameter
r2  - Second Function Parameter
r3  - Return Value
r4  - Free
r5  - Stack Pointer
r6  - Free
r7  - Return Address
*/

constexpr int R_ZERO = 0;
constexpr int R_P1 = 1;
constexpr int R_P2 = 2;
constexpr int R_RET_VAL = 3;
constexpr int R_A = 4;
constexpr int R_SP = 5;
constexpr int R_B = 6;
constexpr int R_RET_ADDR = 7;

Emitter::Emitter() {
  CreateFunc("Start");

  cur_block = CreateBlock("Start");

  CreateData("", "Entry", "Function To be called at program start");

  one = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codegen_context::llvm_context), 1);
  constants[1] = "one";
  CreateData(1, "one");

  neg_one = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codegen_context::llvm_context), -1, true);
  constants[1] = "negOne";
  CreateData(-1, "negOne");

  CreateLw(R_ZERO, R_B, "Entry");
  CreateJalr(R_B, R_RET_ADDR, "", "Call Entry");
  CreateHalt("", "Program Finish");

  // Common Registers for Functions
  int l_reg = 1;
  int r_reg = 2;

  // Shift Left
  CreateFunc("Shift Left");

  cur_block = CreateBlock("shlFun");

  CreateData("shlFun", "shl", "Address for Shift Left");
  int counter_reg = cur_func->reg++;
  int acc_reg = cur_func->reg++;

  CreateAdd(R_ZERO, R_ZERO, R_RET_VAL, "shlFun", "Shift Left Begin");
  CreateAdd(r_reg, R_ZERO, counter_reg, "", "Counter");
  CreateAdd(l_reg, R_ZERO, acc_reg);

  std::string shl_start = mangle("s");
  std::string done = mangle("done");

  cur_block = CreateBlock(shl_start);

  CreateBeq(counter_reg, R_ZERO, done, shl_start, "Condition");
  CreateAdd(acc_reg, acc_reg, acc_reg, "", "Val << 1");
  CreateAdd(acc_reg, R_ZERO, R_RET_VAL);
  CreateAdd(counter_reg, get_virt_reg(neg_one), counter_reg, "", "Counter - 1");
  CreateBeq(R_ZERO, R_ZERO, shl_start);

  cur_block = CreateBlock(done);

  CreateJalr(R_RET_ADDR, R_B, done, "Shift Left End");

  // Shift Right
  CreateFunc("Shift Right");
  cur_block = CreateBlock("shrFun");

  CreateData("shrFun", "shr", "Address for Shift Right");

  // Arithmetic Shift Right
  CreateFunc("Arithmetic Shift Right");
  cur_block = CreateBlock("ashFun");

  CreateData("ashFun", "ashr", "Address for Arithemtic Shift Right");
}

std::string Emitter::mangle(std::string label, bool shorten) {
  static std::unordered_map<std::string, uint8_t> counter;

  if (shorten) {
    // If labels are too long ensures they can properly fit within LC2K limits
    label = label.substr(0, 3);
  }

  std::string mangled = std::format("{}{}", label, counter[label]++);

  if (mangled.size() > 6) {
    fatal_error("Mangled Label larger than LC2K limits");
  }

  return mangled;
}

int size_in_words(llvm::Type *ty) {
  if (ty->isIntegerTy()) {
    constexpr double bits_per_byte = 8.0;
    constexpr double bytes_per_word = 4.0;
    constexpr double bits_per_word = bits_per_byte * bytes_per_word;
    return ceil(ty->getIntegerBitWidth() / (bits_per_word));
  }
  fatal_error("Not Yet Implemented");
}

int Emitter::get_virt_reg(llvm::Value *val) {
  if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
    int32_t const_int = ci->getSExtValue();
    if (const_int == 0) return 0;

    auto it = constants.find(const_int);
    std::string label;
    if (it == constants.end()) {
      label = mangle("c");
      constants[const_int] = label;
      CreateData(const_int, label);
    }
    else {
      label = it->second;
    }

    int virt_reg = cur_func->reg++;
    CreateLw(R_ZERO, virt_reg, label);
    cur_func->virtual_registers[val] = virt_reg;

    return virt_reg;
  }

  auto it = cur_func->virtual_registers.find(val);
  if (it != cur_func->virtual_registers.end()) return it->second;

  int virt_reg = cur_func->reg++;
  cur_func->virtual_registers[val] = virt_reg;

  return virt_reg;
}

int Emitter::compareEQ(llvm::Value *v1, llvm::Value *v2) {
  int l_reg = get_virt_reg(v1);
  int r_reg = get_virt_reg(v2);
  int dest_reg = cur_func->reg++;

  int neg_r = cur_func->reg++;
  CreateNor(r_reg, r_reg, neg_r, "", "Begin ==");
  int one_reg = get_virt_reg(one);
  int two_com = cur_func->reg++;
  CreateAdd(neg_r, one_reg, two_com, "", "~R + 1");
  int sub_reg = cur_func->reg++;
  CreateAdd(l_reg, two_com, sub_reg, "", "L - R");
  std::string if_zero = mangle("z");
  CreateBeq(sub_reg, R_ZERO, if_zero, "L == R ");
  std::string done = mangle("d");
  CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "L != R");
  CreateBeq(R_ZERO, R_ZERO, done);
  CreateAdd(R_ZERO, one_reg, dest_reg);
  CreateNoop(done, "End ==");

  return dest_reg;
}

void Emitter::visitAdd(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  CreateAdd(l_reg, r_reg, dest_reg, "", "Add");
}

void Emitter::visitSub(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  int negate_reg = cur_func->reg++;
  CreateNor(r_reg, r_reg, negate_reg);
  CreateAdd(negate_reg, get_virt_reg(one), negate_reg);
  CreateAdd(l_reg, negate_reg, dest_reg);
}

void Emitter::visitMul(llvm::BinaryOperator &I) {
  (void)I;
  fatal_error("Multiplication Not Yet implemented");
}
void Emitter::visitUDiv(llvm::BinaryOperator &I) {
  (void)I;
  fatal_error("Unsigned Division Not Yet implemented");
}
void Emitter::visitSDiv(llvm::BinaryOperator &I) {
  (void)I;
  fatal_error("Signed Division Not Yet implemented");
}

void Emitter::visitAnd(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  int l_neg_reg = cur_func->reg++;
  int r_neg_reg = cur_func->reg++;

  CreateNor(l_reg, l_reg, l_neg_reg, "", "And Start");
  CreateNor(r_reg, r_reg, r_neg_reg);
  CreateNor(l_neg_reg, r_neg_reg, dest_reg, "", "And End");
}
void Emitter::visitOr(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  int neg_reg = cur_func->reg++;

  CreateNor(l_reg, r_reg, neg_reg, "", "Or Start");
  CreateNor(neg_reg, neg_reg, dest_reg, "", "Or End");
}
void Emitter::visitXor(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  int l_neg_reg = cur_func->reg++;
  int r_neg_reg = cur_func->reg++;
  int nor_1_reg = cur_func->reg++;

  int nor_2_reg = cur_func->reg++;

  CreateNor(l_reg, l_reg, l_neg_reg, "", "Xor Start");
  CreateNor(r_reg, r_reg, r_neg_reg);
  CreateNor(l_neg_reg, r_neg_reg, nor_1_reg);

  CreateNor(l_reg, r_reg, nor_2_reg);

  CreateNor(nor_1_reg, nor_2_reg, dest_reg, "", "Xor End");
}

void Emitter::visitShl(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  CreateAdd(l_reg, 0, 1, "");
  CreateAdd(r_reg, 0, 2, "");
  CreateLw(0, 6, "shl");
  CreateJalr(6, 7, "Call Shift Left");
  CreateAdd(3, 0, dest_reg, "", "Move Result into Destination Reg");
}

void Emitter::visitShr(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  CreateAdd(l_reg, 0, 1, "");
  CreateAdd(r_reg, 0, 2, "");
  CreateLw(0, 6, "shr");
  CreateJalr(6, 7, "Call Shift Right");
  CreateAdd(3, 0, dest_reg, "", "Move Result into Destination Reg");
}

void Emitter::visitAshr(llvm::BinaryOperator &I) {
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);
  llvm::Value *dest = &I;

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(dest);

  CreateAdd(l_reg, 0, 1, "");
  CreateAdd(r_reg, 0, 2, "");
  CreateLw(0, 6, "ashr");
  CreateJalr(6, 7, "Call Arithmetic Shift Right");
  CreateAdd(3, 0, dest_reg, "", "Move Result into Destination Reg");
}

void Emitter::visitAllocaInst(llvm::AllocaInst &I) {
  auto size_words = size_in_words(I.getType());

  cur_func->alloca_offsets[&I] = cur_func->stack_size;
  cur_func->stack_size += size_words;

  cur_func->virtual_registers[&I] = get_virt_reg(&I);
}

void Emitter::visitLoadInst(llvm::LoadInst &I) {
  llvm::Value *ptr = I.getPointerOperand();
  llvm::Value *dest = &I;

  int ptr_reg = get_virt_reg(ptr);
  int dest_reg = get_virt_reg(dest);

  CreateLw(ptr_reg, dest_reg, "Stack");
}

void Emitter::visitStoreInst(llvm::StoreInst &I) {
  llvm::Value *val = I.getValueOperand();
  llvm::Value *ptr = I.getPointerOperand();

  int val_reg = get_virt_reg(val);
  int ptr_reg = get_virt_reg(ptr);

  CreateSw(ptr_reg, val_reg, "Stack");
}

void Emitter::visitGetElementPtrInst(llvm::GetElementPtrInst &I) {
  llvm::Value *base = I.getPointerOperand();
  llvm::Value *dest = &I;
  int base_reg = get_virt_reg(base);
  int dest_reg = get_virt_reg(dest);

  CreateAdd(base_reg, R_ZERO, dest_reg, "", "Get Element Ptr Start");

  for (auto it = I.idx_begin(); it != I.idx_end(); ++it) {
    int val_reg = get_virt_reg(*it);
    CreateAdd(dest_reg, val_reg, dest_reg);
  }
  cur_block->text.back()->comment = "Get Element Ptr End";
}

Emitter::LoweredBlock *Emitter::findOrCreateBlock(std::string name, bool set_pred) {
  LoweredBlock *successor = nullptr;
  for (auto &lb : cur_func->blocks) {
    if (lb->label == name) {
      successor = lb.get();
      break;
    }
  }

  if (!successor) {
    successor = CreateBlock(name);
    if (set_pred) successor->predecessors.push_back(cur_block->label);
  }
  return successor;
}

std::string Emitter::branchHelper(llvm::BranchInst &I, int succ_num) {
  llvm::BasicBlock *successor_bb = I.getSuccessor(succ_num);
  // TODO Ensure all CFG names are appended with "__" to denote names that need to be mangled
  std::string succ_name = "__" + successor_bb->getName().str();
  cur_block->successors.push_back(succ_name);

  findOrCreateBlock(succ_name);

  return succ_name;
}

void Emitter::visitBranchInst(llvm::BranchInst &I) {
  int cond_result = R_ZERO;
  int next_num = 0;

  // Conditional branches branch to false first since it's easier to compare to to 0
  if (I.isConditional()) {
    llvm::Value *condition = I.getCondition();
    cond_result = get_virt_reg(condition);
    next_num = 1;
  }

  std::string succ_name = branchHelper(I, next_num);
  CreateBeq(cond_result, R_ZERO, succ_name);
  if (I.isUnconditional()) return;

  succ_name = branchHelper(I, 0);
  CreateBeq(R_ZERO, R_ZERO, succ_name);
}

void Emitter::visitSwitchInst(llvm::SwitchInst &I) {
  // TODO is jump table possible?
  std::string default_location = "__" + I.getDefaultDest()->getName().str();
  cur_block->successors.push_back(default_location);

  llvm::Value *cond = I.getCondition();

  auto c = I.case_begin();

  while (c != I.case_end()) {
    int cond_reg = compareEQ(cond, c->getCaseValue());
    std::string case_name = "__" + c->getCaseSuccessor()->getName().str();
    cur_block->successors.push_back(case_name);
    CreateBeq(cond_reg, R_ZERO, case_name);
    findOrCreateBlock(case_name);
    ++c;
  }

  CreateBeq(R_ZERO, R_ZERO, default_location);
  findOrCreateBlock(default_location);
}

void Emitter::visitReturnInst(llvm::ReturnInst &I) {
  if (llvm::Value *val = I.getReturnValue()) {
    CreateAdd(get_virt_reg(val), R_ZERO, R_RET_VAL);
  }

  CreateJalr(R_RET_ADDR, R_B, "", "Return");
}

void Emitter::visitPHINode(llvm::PHINode &I) {
  int phi_reg = get_virt_reg(&I);

  for (unsigned int i = 0; i < I.getNumIncomingValues(); ++i) {
    int incoming_reg = get_virt_reg(I.getIncomingValue(i));
    std::string incoming_name = "__" + I.getIncomingBlock(i)->getName().str();
    cur_block->predecessors.push_back(incoming_name);

    LoweredBlock *incoming_block = findOrCreateBlock(incoming_name);
    incoming_block->phi_nodes.push_back(std::make_pair(incoming_reg, phi_reg));
  }
}

void Emitter::visitICmpInst(llvm::ICmpInst &I) {
  llvm::ICmpInst::Predicate pred = I.getPredicate();
  llvm::Value *lhs = I.getOperand(0);
  llvm::Value *rhs = I.getOperand(1);

  int l_reg = get_virt_reg(lhs);
  int r_reg = get_virt_reg(rhs);
  int dest_reg = get_virt_reg(&I);

  switch (pred) {
    case llvm::ICmpInst::Predicate::ICMP_EQ: {
      compareEQ(lhs, rhs);
    } break;
    case llvm::ICmpInst::Predicate::ICMP_NE: {
      int neg_r = cur_func->reg++;
      CreateNor(r_reg, r_reg, neg_r, "", "Begin !=");
      int one_reg = get_virt_reg(one);
      int two_com = cur_func->reg++;
      CreateAdd(neg_r, one_reg, two_com, "", "~R + 1");
      int sub_reg = cur_func->reg++;
      CreateAdd(l_reg, two_com, sub_reg, "", "L - R");
      std::string if_zero = mangle("z");
      CreateBeq(sub_reg, R_ZERO, if_zero, "L == R ");
      std::string done = mangle("d");
      CreateAdd(one_reg, R_ZERO, dest_reg, "", "L != R");
      CreateBeq(R_ZERO, R_ZERO, done);
      CreateAdd(R_ZERO, R_ZERO, dest_reg);
      CreateNoop(done, "End !=");
    } break;
    case llvm::ICmpInst::Predicate::ICMP_UGT:
      CreateNoop("", "TODO Unsigned >");

      break;
    case llvm::ICmpInst::Predicate::ICMP_UGE:
      CreateNoop("", "TODO Unsigned >=");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    case llvm::ICmpInst::Predicate::ICMP_ULT:
      CreateNoop("", "TODO Unsigned <");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    case llvm::ICmpInst::Predicate::ICMP_ULE:
      CreateNoop("", "TODO Unsigned <=");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    case llvm::ICmpInst::Predicate::ICMP_SGT:
      CreateNoop("", "TODO Signed >");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    case llvm::ICmpInst::Predicate::ICMP_SGE:
      CreateNoop("", "TODO Signed >=");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    case llvm::ICmpInst::Predicate::ICMP_SLT:
      CreateNoop("", "TODO Signed <");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    case llvm::ICmpInst::Predicate::ICMP_SLE:
      CreateNoop("", "TODO Signed <=");
      CreateAdd(R_ZERO, R_ZERO, dest_reg, "", "End Comparision");
      break;
    default:
      fatal_error("Unexpected Predicate");
  }
}

void Emitter::pushStack(llvm::Value *val) {
  int val_reg = get_virt_reg(val);

  // TODO push values greater than 1 word onto stack

  int one_reg = get_virt_reg(one);
  CreateSw(R_SP, val_reg, "Stack");
  CreateAdd(R_SP, one_reg, R_SP, "", "SP + 1");
}

int Emitter::popStack(bool collect) {
  int neg_one_reg = get_virt_reg(neg_one);
  CreateAdd(R_SP, neg_one_reg, R_SP, "", "SP - 1");

  if (!collect) return 0;

  int collect_reg = cur_func->reg++;
  CreateLw(R_SP, collect_reg, "Stack");

  return collect_reg;
}

void Emitter::visitCallInst(llvm::CallInst &I) {
  llvm::Function *called_func = I.getCalledFunction();

  auto args = I.arg_begin();

  if (I.arg_size() == 1) {
    int arg_1 = get_virt_reg(*args++);
    CreateAdd(arg_1, R_ZERO, R_P1, "", "Move Val into Param 1");
  }
  if (I.arg_size() == 2) {
    int arg_2 = get_virt_reg(*args++);
    CreateAdd(arg_2, R_ZERO, R_P1, "", "Move Val into Param 2");
  }

  int num_pops = 0;

  while (args != I.arg_end()) {
    pushStack(*args++);
    num_pops++;
  }

  // Indirect call
  if (!called_func) {
    int call_ptr = get_virt_reg(I.getCalledOperand());
    CreateAdd(call_ptr, R_ZERO, R_B);
  }
  else {
    std::string func_addr = "__" + called_func->getName().str();

    CreateLw(R_ZERO, R_B, func_addr);
  }

  CreateJalr(R_B, R_RET_ADDR);

  int dest_reg = get_virt_reg(&I);
  CreateAdd(R_RET_VAL, R_ZERO, dest_reg, "", "Move Return Val into Dest Reg");

  while (num_pops > 0) {
    popStack();
    num_pops--;
  }
}

void Emitter::visitSelectInst(llvm::SelectInst &I) {
  int dest_reg = get_virt_reg(&I);
  int cond_reg = get_virt_reg(I.getCondition());

  std::string false_dest = mangle("f");
  std::string done = mangle("d");

  CreateBeq(cond_reg, R_ZERO, false_dest, "", "Select Start");
  int true_reg = get_virt_reg(I.getTrueValue());
  CreateAdd(true_reg, R_ZERO, dest_reg, "", "True");
  CreateBeq(R_ZERO, R_ZERO, done);

  CreateNoop(false_dest);
  int false_reg = get_virt_reg(I.getFalseValue());
  CreateAdd(false_reg, R_ZERO, dest_reg, "", "False");

  CreateNoop(done);
}

void Emitter::visitCastInst(llvm::CastInst &I) {
  (void)I;

  llvm::errs() << "Unhandled: Cast " << get_virt_reg(I.getOperand(0)) << " -> " << get_virt_reg(&I) << " ("
               << I.getOpcodeName() << ")\n";

  CreateNoop("", "Unhandled Cast");
}

void Emitter::visitInstruction(llvm::Instruction &I) { llvm::errs() << "Unhandled: " << I << "\n"; }

void Emitter::debug_emit(std::ostream &os) {
  for (auto &func : funcs) {
    os << "\n\nFunction " << func->name << "\n";
    for (auto &block : func->blocks) {
      os << "\n" << block->label << ":\n";
      for (auto &t : block->text) {
        os << "\t" << t->emit();
      }
    }
    os << ";\n";
  }

  for (auto &d : data) {
    os << d->emit();
  }
}

}  // namespace LC2K
}  // namespace JCC
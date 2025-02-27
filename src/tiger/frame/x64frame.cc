#include "tiger/frame/x64frame.h"
#include "tiger/env/env.h"

#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

extern frame::RegManager *reg_manager;
extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;

namespace frame {

X64RegManager::X64RegManager() : RegManager() {
  for (int i = 0; i < REG_COUNT; i++)
    regs_.push_back(temp::TempFactory::NewTemp());

  // Note: no frame pointer in tiger compiler
  std::array<std::string_view, REG_COUNT> reg_name{
      "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp",
      "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"};
  int reg = RAX;
  for (auto &name : reg_name) {
    temp_map_->Enter(regs_[reg], new std::string(name));
    reg++;
  }
}

temp::TempList *X64RegManager::Registers() {
  const std::array reg_array{
      RAX, RBX, RCX, RDX, RSI, RDI, RBP, R8, R9, R10, R11, R12, R13, R14, R15,
  };
  auto *temp_list = new temp::TempList();
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

temp::TempList *X64RegManager::ArgRegs() {
  const std::array reg_array{RDI, RSI, RDX, RCX, R8, R9};
  auto *temp_list = new temp::TempList();
  ;
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

temp::TempList *X64RegManager::CallerSaves() {
  std::array reg_array{RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11};
  auto *temp_list = new temp::TempList();
  ;
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

temp::TempList *X64RegManager::CalleeSaves() {
  std::array reg_array{RBP, RBX, R12, R13, R14, R15};
  auto *temp_list = new temp::TempList();
  ;
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

temp::TempList *X64RegManager::ReturnSink() {
  temp::TempList *temp_list = CalleeSaves();
  temp_list->Append(regs_[SP]);
  temp_list->Append(regs_[RV]);
  return temp_list;
}

int X64RegManager::WordSize() { return 8; }

temp::Temp *X64RegManager::FramePointer() { return regs_[FP]; }

class InFrameAccess : public Access {
public:
  int offset;
  frame::Frame *parent_frame;

  explicit InFrameAccess(int offset, frame::Frame *parent)
      : offset(offset), parent_frame(parent) {}

  /* TODO: Put your lab5-part1 code here */
  llvm::Value *ToLLVMVal(llvm::Value *frame_addr_ptr) const override {
    llvm::Value *frame_size = ir_builder->CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()),
      parent_frame->framesize_global
    );
    llvm::Value *off = ir_builder->CreateAdd(
      frame_size,
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), offset)
    );
    llvm::Value *varAddr = ir_builder->CreateAdd(frame_addr_ptr, off);
    return varAddr;
  }
};

class X64Frame : public Frame {
public:
  X64Frame(temp::Label *name, std::list<frame::Access *> *formals)
      : Frame(8, 0, name, formals) {}

  [[nodiscard]] std::string GetLabel() const override { return name_->Name(); }
  [[nodiscard]] temp::Label *Name() const override { return name_; }
  [[nodiscard]] std::list<frame::Access *> *Formals() const override {
    return formals_;
  }
  frame::Access *AllocLocal(bool escape) override {
    frame::Access *access;

    offset_ -= reg_manager->WordSize();
    access = new InFrameAccess(offset_, this);

    return access;
  }
  void AllocOutgoSpace(int size) override {
    if (size > outgo_size_)
      outgo_size_ = size;
  }
};

frame::Frame *NewFrame(temp::Label *name, std::list<bool> formals) {
  /* TODO: Put your lab5-part1 code here */
  std::list<frame::Access *> *formal_accesses = new std::list<frame::Access *>;
  frame::Frame *frame = new X64Frame(name, formal_accesses);

  // update arguments' InFrameAccess
  int offset = reg_manager->WordSize();
  for (bool escape : formals) {
    frame::InFrameAccess *access = new frame::InFrameAccess(offset, frame);
    frame->formals_->push_back(access);
    offset += reg_manager->WordSize();
  }

  // define frame size
  std::string frame_size_name = name->Name() + "_framesize_global";
  frame->framesize_global = (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(
    frame_size_name, llvm::Type::getInt64Ty(ir_module->getContext())
  );

  return frame;
}

/**
 * Moving incoming formal parameters, the saving and restoring of callee-save
 * Registers
 * @param frame curruent frame
 * @param stm statements
 * @return statements with saving, restoring and view shift
 */
assem::InstrList *ProcEntryExit1(std::string_view function_name,
                                 assem::InstrList *body) {
  // TODO: your lab5 code here
  auto callee_regs_list = reg_manager->CalleeSaves()->GetList();
  std::vector<temp::Temp *> tmps;

  // store callee-saved registers at first
  for (auto it = callee_regs_list.begin(); it != callee_regs_list.end(); it++) {
    auto tmp = temp::TempFactory::NewTemp();
    tmps.push_back(tmp);
    body->Insert(body->GetList().begin(), 
      new assem::OperInstr("movq `s0,`d0", new temp::TempList(tmp), new temp::TempList(*it), nullptr));
  }

  // create exit label at last
  body->Append(new assem::LabelInstr(std::string(function_name) + "_exit"));

  // restore callee-saved registers
  for (auto it = callee_regs_list.begin(); it != callee_regs_list.end(); it++) {
    size_t idx = std::distance(callee_regs_list.begin(), it);
    auto tmp = tmps[idx];
    body->Append(
      new assem::OperInstr("movq `s0,`d0", new temp::TempList(*it), new temp::TempList(tmp), nullptr));
  }

  return body;
}

/**
 * Appends a “sink” instruction to the function body to tell the register
 * allocator that certain registers are live at procedure exit
 * @param body function body
 * @return instructions with sink instruction
 */
assem::InstrList *ProcEntryExit2(assem::InstrList *body) {
  body->Append(new assem::OperInstr("", new temp::TempList(),
                                    reg_manager->ReturnSink(), nullptr));
  return body;
}

/**
 * The procedure entry/exit sequences
 * @param frame the frame of current func
 * @param body current function body
 * @return whole instruction list with prolog_ end epilog_
 */
assem::Proc *ProcEntryExit3(std::string_view function_name,
                            assem::InstrList *body) {
  std::string prologue = "";
  std::string epilogue = "";

  // TODO: your lab5 code here
  const std::string func_name = function_name.data();

  prologue += func_name + ":\n";  // label definition of the function name
  prologue += "subq $" + func_name + "_framesize_local,%rsp\n";  // adjust the stack pointer

  epilogue += "addq $" + func_name + "_framesize_local,%rsp\n";  // reset the stack pointer
  epilogue += "retq\n";  // return instruction

  return new assem::Proc(prologue, body, epilogue);
}

void Frags::PushBack(Frag *frag) { frags_.emplace_back(frag); }

} // namespace frame
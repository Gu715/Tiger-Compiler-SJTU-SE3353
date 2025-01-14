#include "tiger/codegen/codegen.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <random>

extern frame::RegManager *reg_manager;
extern frame::Frags *frags;

namespace {

constexpr int maxlen = 1024;

} // namespace

namespace cg {

void CodeGen::Codegen() {
  temp_map_ = new std::unordered_map<llvm::Value *, temp::Temp *>();
  bb_map_ = new std::unordered_map<llvm::BasicBlock *, int>();
  phi_label_ = new std::unordered_set<int>();
  auto *list = new assem::InstrList();

  // firstly get all global string's location
  for (auto &&frag : frags->GetList()) {
    if (auto *str_frag = dynamic_cast<frame::StringFrag *>(frag)) {
      auto tmp = temp::TempFactory::NewTemp();
      list->Append(new assem::OperInstr(
          "leaq " + std::string(str_frag->str_val_->getName()) + "(%rip),`d0",
          new temp::TempList(tmp), new temp::TempList(), nullptr));
      temp_map_->insert({str_frag->str_val_, tmp});
    }
  }

  // move arguments to temp
  auto arg_iter = traces_->GetBody()->arg_begin();
  auto regs = reg_manager->ArgRegs();
  auto tmp_iter = regs->GetList().begin();

  // first arguement is rsp, we need to skip it
  ++arg_iter;

  for (; arg_iter != traces_->GetBody()->arg_end() &&
         tmp_iter != regs->GetList().end();
       ++arg_iter, ++tmp_iter) {
    auto tmp = temp::TempFactory::NewTemp();
    list->Append(new assem::OperInstr("movq `s0,`d0", new temp::TempList(tmp),
                                      new temp::TempList(*tmp_iter), nullptr));
    temp_map_->insert({static_cast<llvm::Value *>(arg_iter), tmp});
  }

  // pass-by-stack parameters
  if (arg_iter != traces_->GetBody()->arg_end()) {
    auto last_sp = temp::TempFactory::NewTemp();
    list->Append(
        new assem::OperInstr("movq %rsp,`d0", new temp::TempList(last_sp),
                             new temp::TempList(reg_manager->GetRegister(
                                 frame::X64RegManager::Reg::RSP)),
                             nullptr));
    list->Append(new assem::OperInstr(
        "addq $" + std::string(traces_->GetFunctionName()) +
            "_framesize_local,`s0",
        new temp::TempList(last_sp),
        new temp::TempList({last_sp, reg_manager->GetRegister(
                                         frame::X64RegManager::Reg::RSP)}),
        nullptr));
    while (arg_iter != traces_->GetBody()->arg_end()) {
      auto tmp = temp::TempFactory::NewTemp();
      list->Append(new assem::OperInstr(
          "movq " +
              std::to_string(8 * (arg_iter - traces_->GetBody()->arg_begin())) +
              "(`s0),`d0",
          new temp::TempList(tmp), new temp::TempList(last_sp), nullptr));
      temp_map_->insert({static_cast<llvm::Value *>(arg_iter), tmp});
      ++arg_iter;
    }
  }

  // construct bb_map
  int bb_index = 0;
  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    bb_map_->insert({bb, bb_index++});
  }

  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    // record every return value from llvm instruction
    for (auto &&inst : bb->getInstList())
      temp_map_->insert({&inst, temp::TempFactory::NewTemp()});
  }

  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    // Generate label for basic block
    list->Append(new assem::LabelInstr(std::string(bb->getName())));

    // Generate instructions for basic block
    for (auto &&inst : bb->getInstList())
      InstrSel(list, inst, traces_->GetFunctionName(), bb);
  }

  assem_instr_ = std::make_unique<AssemInstr>(frame::ProcEntryExit2(
      frame::ProcEntryExit1(traces_->GetFunctionName(), list)));
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}

void CodeGen::InstrSel(assem::InstrList *instr_list, llvm::Instruction &inst,
                       std::string_view function_name, llvm::BasicBlock *bb) {
  // TODO: your lab5 code here
  switch (inst.getOpcode()) {
  case llvm::Instruction::Load: {
    llvm::LoadInst *load_inst = llvm::dyn_cast<llvm::LoadInst>(&inst);
    llvm::Value *val = load_inst->getPointerOperand();
    temp::Temp *dst = temp_map_->at(load_inst);
    if (auto *global_var = llvm::dyn_cast<llvm::GlobalVariable>(val)) {  // is global variable
      std::string var_name = global_var->getName().str();
      instr_list->Append(
        new assem::OperInstr(
          "movq " + var_name + "(%rip),`d0",  // global variable needs %rip to locate
          new temp::TempList(dst), new temp::TempList(), nullptr
        )
      );
    } else {
      instr_list->Append(
        new assem::OperInstr(
          "movq (`s0),`d0",   // load from mem to reg
          new temp::TempList(dst), new temp::TempList(temp_map_->at(val)), nullptr
        )
      );
    }
    
    break;
  }

  case llvm::Instruction::Store: {
    llvm::StoreInst *store_inst = llvm::dyn_cast<llvm::StoreInst>(&inst);
    llvm::Value *val = store_inst->getValueOperand();
    temp::Temp *src = temp_map_->at(store_inst->getPointerOperand());
    if (llvm::ConstantInt *const_int = llvm::dyn_cast<llvm::ConstantInt>(val)) {  // is immediate
      int imm_val = const_int->getSExtValue();
      instr_list->Append(
        new assem::OperInstr(
          "movq $" + std::to_string(imm_val) + ",(`s0)",
          new temp::TempList(), new temp::TempList(src), nullptr  // fix lab5: s0 is src
        )
      );
    } else {
      instr_list->Append(
        new assem::OperInstr(
          "movq `s0,(`s1)",  // store from reg to mem
          new temp::TempList(), 
          new temp::TempList({temp_map_->at(val), src}), nullptr  // fix lab5: both s0 and s1 are src
        )
      );
    }
    break;
  }

  case llvm::Instruction::PtrToInt:
  case llvm::Instruction::IntToPtr:
  case llvm::Instruction::ZExt: {
    llvm::Value *val = inst.getOperand(0);
    temp::Temp *src = temp_map_->at(val);
    temp::Temp *dst = temp_map_->at(&inst);
    instr_list->Append(
      new assem::MoveInstr(
        "movq `s0,`d0", new temp::TempList(dst), new temp::TempList(src)
      )
    );
    break;
  }

  case llvm::Instruction::GetElementPtr: {
    llvm::GetElementPtrInst *gep_inst = llvm::dyn_cast<llvm::GetElementPtrInst>(&inst);
    const llvm::DataLayout &data_layout = inst.getFunction()->getParent()->getDataLayout();

    switch (gep_inst->getNumOperands()) {
    case 2: {  // get array element pointer
      llvm::Value *base = inst.getOperand(0);
      llvm::Value *index = inst.getOperand(1);
      temp::Temp *src0 = temp_map_->at(base);
      temp::Temp *src1 = temp_map_->at(index);
      temp::Temp *dst = temp_map_->at(&inst);
      instr_list->Append(
        new assem::OperInstr(
          "leaq (`s0,`s1,8),`d0", // note: scale is always 8 because every instruction operates with quadword
          // if array type is int32, we treat it as int64
          new temp::TempList(dst), new temp::TempList({src0, src1}), nullptr
        )
      );
      break;
    }
    case 3: {  // get field pointer
      llvm::Value *base = inst.getOperand(0);
      llvm::Value *offset = inst.getOperand(1);
      llvm::Value *index = inst.getOperand(2);
      llvm::StructType *struct_type = llvm::dyn_cast<llvm::StructType>(gep_inst->getSourceElementType());
      const llvm::StructLayout *struct_layout = data_layout.getStructLayout(struct_type);
      int index_val = llvm::dyn_cast<llvm::ConstantInt>(index)->getSExtValue();
      uint64_t field_offset = index_val * 8;  // fix lab5: always align 8 in x86-64
      int offset_val = llvm::dyn_cast<llvm::ConstantInt>(offset)->getSExtValue();
      assert(offset_val == 0);
      temp::Temp *dst = temp_map_->at(&inst);
      std::string assem;
      if (field_offset == 0) {
        assem = "leaq (`s0),`d0";
      } else {
        assem = "leaq " + std::to_string(field_offset) + "(`s0),`d0";
      }
      instr_list->Append(
        new assem::OperInstr(
          assem, new temp::TempList(dst), new temp::TempList(temp_map_->at(base)), nullptr
        )
      );
      break;
    }
    default: assert(0);
    }

    break;
  }

  case llvm::Instruction::Add:
  case llvm::Instruction::Sub:
  case llvm::Instruction::Mul: {
    if (IsRsp(&inst, function_name)) {  // means this instruction adjust stack pointer at the beginning of function body
      assert(inst.getOpcode() == llvm::Instruction::Sub);  // stack pointer is adjusted by subtraction
      // instr_list->Append(
      //   new assem::OperInstr(  // subq framesize, %rsp
      //     "subq `s0,`d0", 
      //     new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)),
      //     new temp::TempList({temp_map_->at(inst.getOperand(1)),  // fix lab5: s0 is both src and dst
      //                         reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}), nullptr
      //   )
      // );
      // fix lab5: we adjust stack pointer in ProcExit3
      instr_list->Append(
        new assem::OperInstr(  // move %rsp to the reg of %func_sp, so we don't need to handle load %func_sp
          "movq `s0,`d0", new temp::TempList(temp_map_->at(&inst)),
          new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)),
          nullptr
        )
      );
      break;
    }
    llvm::Value *lhs = inst.getOperand(0);
    llvm::Value *rhs = inst.getOperand(1);
    temp::Temp *dst = temp_map_->at(&inst);

    // first move lhs to dst
    if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(lhs)) {
      int imm_val = const_int->getSExtValue();
      instr_list->Append(
        new assem::OperInstr(
          "movq $" + std::to_string(imm_val) + ",`d0",
          new temp::TempList(dst), new temp::TempList(), nullptr
        )
      );
    } else {
      instr_list->Append(
        new assem::MoveInstr(
          "movq `s0,`d0", new temp::TempList(dst),
          new temp::TempList(temp_map_->at(lhs))
        )
      );
    }
    
    std::string op_name;
    switch (inst.getOpcode()) {
      case llvm::Instruction::Add: op_name = "addq"; break;
      case llvm::Instruction::Sub: op_name = "subq"; break;
      case llvm::Instruction::Mul: op_name = "imulq"; break;
      default: assert(0);
    }

    // then calculate with rhs
    if (llvm::ConstantInt *const_int = llvm::dyn_cast<llvm::ConstantInt>(rhs)) {
      int imm_val = const_int->getSExtValue();
      instr_list->Append(
        new assem::OperInstr(
          op_name + " $" + std::to_string(imm_val) + ",`d0",
          new temp::TempList(dst), new temp::TempList(dst), nullptr  // fix lab5: d0 is both dst and src
        )
      );
    } else {
      instr_list->Append(
        new assem::OperInstr(
          op_name + " `s0,`d0", new temp::TempList(dst), 
          new temp::TempList({temp_map_->at(rhs), dst}), nullptr  // fix lab5: d0 is both dst and src
        )
      );
    }

    break;
  }

  /**
   * idivq:
   * R[%rax] <- R[%rdx]:R[%rax]  ÷  S
   * R[%rdx] <- R[%rdx]:R[%rax] mod S
   */
  case llvm::Instruction::SDiv: {
    llvm::Value *lhs = inst.getOperand(0);
    llvm::Value *rhs = inst.getOperand(1);
    temp::Temp *dst = temp_map_->at(&inst);
    instr_list->Append(
      new assem::MoveInstr(
        "movq `s0,`d0", 
        new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
        new temp::TempList(temp_map_->at(lhs))
      )
    );
    instr_list->Append(
      new assem::OperInstr(
        "cqto",  // cqto set %rdx to the sign of %rax
        new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RDX)}),  // fix lab5: 增加了src和dst
        new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)), 
        nullptr
      )
    );
    temp::Temp *src;
    if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(rhs)) {
      int imm_val = const_int->getSExtValue();
      src = temp::TempFactory::NewTemp();
      instr_list->Append(
        new assem::OperInstr(
          "movq $" + std::to_string(imm_val) + ",`d0",  // use a temp reg to store imm
          new temp::TempList(src), new temp::TempList(), nullptr
        )
      );
    } else {
      src = temp_map_->at(rhs);
    }
    instr_list->Append(
      new assem::OperInstr(
        "idivq `s0", 
        new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::RAX),  // fix lab5: add rdx and rax
                            reg_manager->GetRegister(frame::X64RegManager::RDX)}), 
        new temp::TempList({src,
                            reg_manager->GetRegister(frame::X64RegManager::RAX),
                            reg_manager->GetRegister(frame::X64RegManager::RDX)}), nullptr
      )
    );
    instr_list->Append(
      new assem::MoveInstr(
        "movq `s0,`d0", new temp::TempList(dst), 
        new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX))
      )
    );
    break;
  }

  case llvm::Instruction::Call: {
    llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(&inst);
    auto arg_iter = call_inst->arg_begin();
    auto regs_list = reg_manager->ArgRegs()->GetList();
    auto tmp_iter = regs_list.begin();
    if (!call_inst->getCalledFunction()->isDeclaration()) {
      arg_iter++;  // if the called function is defined by us, first arguement is rsp, we need to skip it
    }
    temp::TempList *param_reg = new temp::TempList();  // store which parameter registers are used
    // move arguments to reg
    for (; arg_iter != call_inst->arg_end() && tmp_iter != regs_list.end(); 
         arg_iter++, tmp_iter++) {
      if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(*arg_iter)) {
        param_reg->Append(*tmp_iter);
        int imm_val = const_int->getSExtValue();
        instr_list->Append(
          new assem::OperInstr(
            "movq $" + std::to_string(imm_val) + ",`d0",
            new temp::TempList(*tmp_iter), new temp::TempList(), nullptr
          )
        );
      } else {
        instr_list->Append(
          new assem::OperInstr(
            "movq `s0,`d0", new temp::TempList(*tmp_iter),
            new temp::TempList(temp_map_->at(*arg_iter)), nullptr
          )
        );
      }
    }

    // pass the rest arguments by stack
    while (arg_iter != call_inst->arg_end()) {
      if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(*arg_iter)) {
        int imm_val = const_int->getSExtValue();
        instr_list->Append(
          new assem::OperInstr(
            "movq $" + std::to_string(imm_val) + "," + 
              std::to_string(reg_manager->WordSize() * (arg_iter - call_inst->arg_begin())) + "(`d0)",
            new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)), 
            new temp::TempList(), nullptr
          )
        );
      } else {
        instr_list->Append(
          new assem::OperInstr(
            "movq `s0," + std::to_string(reg_manager->WordSize() * (arg_iter - call_inst->arg_begin())) + "(`d0)",
            new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)), 
            new temp::TempList(temp_map_->at(*arg_iter)), nullptr
          )
        );
      }
      arg_iter++;
    }

    instr_list->Append(
      new assem::OperInstr(
        "call " + call_inst->getCalledFunction()->getName().str(),
        reg_manager->CallerSaves(), param_reg, nullptr  // fix lab5: add src and dst
      )
    );

    if (!call_inst->getType()->isVoidTy()) {
      // if the called function has return value, get it from %rax
      instr_list->Append(
        new assem::MoveInstr(
          "movq `s0,`d0", new temp::TempList(temp_map_->at(&inst)),
          new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX))
        )
      );
    }

    break;
  }

  case llvm::Instruction::Ret: {
    llvm::ReturnInst *ret_inst = llvm::dyn_cast<llvm::ReturnInst>(&inst);
    llvm::Value *ret_val = ret_inst->getReturnValue();
    if (ret_val && !ret_val->getType()->isVoidTy()) {
      // move the returned value to %rax
      if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(ret_val)) {
        int imm_val = const_int->getSExtValue();
        instr_list->Append(
          new assem::OperInstr(
            "movq $" + std::to_string(imm_val) + ",`d0",
            new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
            new temp::TempList(), nullptr
          )
        );
      } else {
        instr_list->Append(
          new assem::MoveInstr(
            "movq `s0,`d0", 
            new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
            new temp::TempList(temp_map_->at(ret_val))
          )
        );
      }
    }

    std::string_view exit_label_name(std::string(function_name) + "_exit");
    temp::Label *exit_label = temp::LabelFactory::NamedLabel(exit_label_name);
    instr_list->Append(
      new assem::OperInstr(
        "jmp `j0", new temp::TempList(), new temp::TempList(),   // jump to the exit label
        new assem::Targets(new std::vector<temp::Label *>{exit_label})
      )
    );

    break;
  }

  case llvm::Instruction::Br: {
    int bbid = bb_map_->at(bb);
    instr_list->Append(
      new assem::OperInstr(
        "movq $" + std::to_string(bbid) + ",`d0",  // move bb index to %rax before every jump, see PHI
        new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
        new temp::TempList(), nullptr
      )
    );

    llvm::BranchInst *br_inst = llvm::dyn_cast<llvm::BranchInst>(&inst);
    if (br_inst->isConditional()) {
      llvm::Value *cond = br_inst->getCondition();
      llvm::BasicBlock *true_block = br_inst->getSuccessor(0);
      llvm::BasicBlock *false_block = br_inst->getSuccessor(1);
      temp::Label *true_label = temp::LabelFactory::NamedLabel(true_block->getName().str());
      temp::Label *false_label = temp::LabelFactory::NamedLabel(false_block->getName().str());

      instr_list->Append(
        new assem::OperInstr(
          "cmpq $0,`s0", new temp::TempList(), new temp::TempList(temp_map_->at(cond)), nullptr
        )
      );
      instr_list->Append(
        new assem::OperInstr(
          "je `j0", new temp::TempList(), new temp::TempList(), 
          new assem::Targets(new std::vector<temp::Label *>{false_label})
        )
      );
      instr_list->Append(
        new assem::OperInstr(
          "jmp `j0", new temp::TempList(), new temp::TempList(), 
          new assem::Targets(new std::vector<temp::Label *>{true_label})
        )
      );
    } else {
      llvm::BasicBlock *target_block = br_inst->getSuccessor(0);
      temp::Label *target_label = temp::LabelFactory::NamedLabel(target_block->getName().str());
      instr_list->Append(
        new assem::OperInstr(
          "jmp `j0", new temp::TempList(), new temp::TempList(), 
          new assem::Targets(new std::vector<temp::Label *>{target_label})
        )
      );
    }
    break;
  }

  case llvm::Instruction::ICmp: {
    llvm::Value *lhs = inst.getOperand(0);
    llvm::Value *rhs = inst.getOperand(1);
    if (auto *const_null = llvm::dyn_cast<llvm::ConstantPointerNull>(rhs)) {
      instr_list->Append(
        new assem::OperInstr(
          "cmpq $0,`s0", new temp::TempList(), new temp::TempList(temp_map_->at(lhs)), nullptr
        )
      );
    } else if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(rhs)) {
      int imm_val = const_int->getSExtValue();
      instr_list->Append(
        new assem::OperInstr(
          "cmpq $" + std::to_string(imm_val) + ",`s0", new temp::TempList(),
          new temp::TempList(temp_map_->at(lhs)), nullptr
        )
      );
    } else {
      instr_list->Append(
        new assem::OperInstr(
          "cmpq `s1,`s0", new temp::TempList(), 
          new temp::TempList({temp_map_->at(lhs), temp_map_->at(rhs)}), nullptr
        )
      );
    }

    llvm::ICmpInst *icmp_inst = llvm::dyn_cast<llvm::ICmpInst>(&inst);
    temp::Temp *dst = temp_map_->at(&inst);
    std::string set_name;
    switch (icmp_inst->getPredicate()) {
      case llvm::CmpInst::ICMP_EQ: set_name = "sete"; break;
      case llvm::CmpInst::ICMP_NE: set_name = "setne"; break;
      case llvm::CmpInst::ICMP_SGT: set_name = "setg"; break;
      case llvm::CmpInst::ICMP_SGE: set_name = "setge"; break;
      case llvm::CmpInst::ICMP_SLT: set_name = "setl"; break;
      case llvm::CmpInst::ICMP_SLE: set_name = "setle"; break;
      default: assert(0);
    }
    instr_list->Append(
      new assem::OperInstr(
        "movq $0,`d0", new temp::TempList(dst), new temp::TempList(), nullptr
      )
    );  // fix lab5: very important!!! 极其重要！！！把目标地址先清0，再set，因为set只会设置低8位！！！
    instr_list->Append(
      new assem::OperInstr(
        set_name + " `d0", new temp::TempList(dst), new temp::TempList(), nullptr
      )
    );

    break;
  }

  case llvm::Instruction::PHI: {
    llvm::PHINode *phi_inst = llvm::dyn_cast<llvm::PHINode>(&inst);
    int num = phi_inst->getNumIncomingValues();
    std::string bb_name = bb->getName().str();

    std::vector<std::string> names;
    for (int i = 0; i < num; i++) {
      llvm::BasicBlock *bb = phi_inst->getIncomingBlock(i);
      int bbid = bb_map_->at(bb);
      std::string label_name;
      {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(0, INT32_MAX);
        int t;
        do {
          t = dis(gen);
        } while (phi_label_->find(t) != phi_label_->end());
        phi_label_->insert(t);
        label_name = bb_name + "_" + std::to_string(t);
        names.push_back(label_name);
      }
      temp::Label *label = temp::LabelFactory::NamedLabel(label_name);
      instr_list->Append(
        new assem::OperInstr(
          "cmpq $" + std::to_string(bbid) + ",`s0",  // use bb index from %rax to know which block it jumps from
          new temp::TempList(), 
          new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
          nullptr
        )
      );
      instr_list->Append(
        new assem::OperInstr(
          "je `j0", new temp::TempList(), new temp::TempList(), 
          new assem::Targets(new std::vector<temp::Label *>{label})
        )
      );
    }

    temp::Temp *dst = temp_map_->at(&inst);
    std::string end_label_name = bb_name + "_end";
    temp::Label *end_label = temp::LabelFactory::NamedLabel(end_label_name);
    for (int i = 0; i < num; i++) {
      llvm::Value *val = phi_inst->getIncomingValue(i);
      std::string label_name = names[i];
      instr_list->Append(new assem::LabelInstr(label_name));
      if (auto *const_int = llvm::dyn_cast<llvm::ConstantInt>(val)) {
        int imm_val = const_int->getSExtValue();
        instr_list->Append(
          new assem::OperInstr(
            "movq $" + std::to_string(imm_val) + ",`d0",
            new temp::TempList(dst), new temp::TempList(), nullptr
          )
        );
      } else if (auto *const_null = llvm::dyn_cast<llvm::ConstantPointerNull>(val)) {
        instr_list->Append(
          new assem::OperInstr(
            "movq $0,`d0", new temp::TempList(dst), new temp::TempList(), nullptr
          )
        );
      } else {
        instr_list->Append(
          new assem::MoveInstr(
            "movq `s0,`d0", new temp::TempList(dst),
            new temp::TempList(temp_map_->at(val))
          )
        );
      }
      instr_list->Append(
        new assem::OperInstr(
          "jmp `j0", new temp::TempList(), new temp::TempList(), 
          new assem::Targets(new std::vector<temp::Label *>{end_label})
        )
      );
    }
    instr_list->Append(new assem::LabelInstr(end_label_name));

    break;
  }

  default:
    throw std::runtime_error(std::string("Unknown instruction: ") +
                             inst.getOpcodeName());
  }
}

} // namespace cg
#include "tiger/translate/translate.h"

#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/x64frame.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <stack>
#include <map>

extern frame::Frags *frags;
extern frame::RegManager *reg_manager;
extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;
std::stack<llvm::Function *> func_stack;
std::stack<llvm::BasicBlock *> loop_stack;
llvm::Function *alloc_record;
llvm::Function *init_array;
llvm::Function *string_equal;
std::vector<std::pair<std::string, frame::Frame *>> frame_info;


std::map<std::string, llvm::Function *> functions;

bool CheckBBTerminatorIsBranch(llvm::BasicBlock *bb) {
  auto inst = bb->getTerminator();
  if (inst) {
    llvm::BranchInst *branchInst = llvm::dyn_cast<llvm::BranchInst>(inst);
    if (branchInst && !branchInst->isConditional()) {
      return true;
    }
  }
  return false;
}

int getActualFramesize(tr::Level *level) {
  return level->frame_->calculateActualFramesize();
}


namespace tr {

Access *Access::AllocLocal(Level *level, bool escape) {
  return new Access(level, level->frame_->AllocLocal(escape));
}

Level *Level::newLevel(Level *parent, temp::Label *name, std::list<bool> formals) {
  formals.push_front(true);
  frame::Frame *frame = frame::NewFrame(name, formals);
  return new Level(frame, parent);
}

class ValAndTy {
public:
  type::Ty *ty_;
  llvm::Value *val_;

  // MY_MODIFY: init with last_bb
  ValAndTy(llvm::Value *val, type::Ty *ty, llvm::BasicBlock *last_bb = nullptr) : 
    val_(val), ty_(ty), last_bb_(last_bb) {} 
};

void ProgTr::OutputIR(std::string_view filename) {
  std::string llvmfile = std::string(filename) + ".ll";
  std::error_code ec;
  llvm::raw_fd_ostream out(llvmfile, ec, llvm::sys::fs::OpenFlags::OF_Text);
  ir_module->print(out, nullptr);
}

void ProgTr::Translate() {
  FillBaseVEnv();
  FillBaseTEnv();
  /* TODO: Put your lab5-part1 code here */

  // declare functions
  llvm::FunctionType *alloc_record_func_type = llvm::FunctionType::get(
    llvm::Type::getInt64Ty(ir_module->getContext()),
    {llvm::Type::getInt32Ty(ir_module->getContext())},
    false
  );
  alloc_record = llvm::Function::Create(
    alloc_record_func_type, 
    llvm::Function::ExternalLinkage,
    "alloc_record",
    ir_module
  );

  llvm::FunctionType *init_array_func_type = llvm::FunctionType::get(
    llvm::Type::getInt64Ty(ir_module->getContext()),
    {
      llvm::Type::getInt32Ty(ir_module->getContext()), 
      llvm::Type::getInt64Ty(ir_module->getContext())
    },
    false
  );
  init_array = llvm::Function::Create(
    init_array_func_type,
    llvm::Function::ExternalLinkage,
    "init_array",
    ir_module
  );

  llvm::FunctionType *string_equal_func_type = llvm::FunctionType::get(
    llvm::Type::getInt1Ty(ir_module->getContext()),
    {
      type::StringTy::Instance()->GetLLVMType(),
      type::StringTy::Instance()->GetLLVMType()
    },
    false
  );
  string_equal = llvm::Function::Create(
    string_equal_func_type,
    llvm::Function::ExternalLinkage,
    "string_equal",
    ir_module
  );

  // define main function
  std::vector<llvm::Type*> paramTypes = {
    llvm::Type::getInt64Ty(ir_module->getContext()),
    llvm::Type::getInt64Ty(ir_module->getContext()),
  };
  llvm::FunctionType *main_type = llvm::FunctionType::get(
    llvm::Type::getInt32Ty(ir_module->getContext()), paramTypes, false
  );
  std::string main_name = main_level_->frame_->GetLabel();
  llvm::Function *main = llvm::Function::Create(
    main_type, llvm::Function::ExternalLinkage, main_name, ir_module
  );
  functions.insert(std::make_pair(main_name, main));
  frame_info.push_back({main_name, main_level_->frame_});

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(
    ir_module->getContext(), main_name, main
  );
  ir_builder->SetInsertPoint(entry);

  // set stack pointer
  auto args = main->args();
  llvm::Argument &first_arg = *args.begin();
  llvm::Value *val = &first_arg;
  std::string sp_name = main_level_->frame_->name_->Name() + "_sp";
  llvm::Value *frame_sz = ir_builder->CreateLoad(
    llvm::Type::getInt64Ty(ir_module->getContext()), main_level_->frame_->framesize_global
  );
  llvm::Value *sp = ir_builder->CreateSub(val, frame_sz, sp_name);
  main_level_->set_sp(sp);

  // translate
  this->absyn_tree_->Translate(venv_.get(), tenv_.get(), main_level_.get(), errormsg_.get());

  // return 0
  ir_builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0));

  // calculate frame size for all frames
  for (auto &frame_pair : frame_info) {
    frame::Frame *frame = frame_pair.second;
    int frame_size = frame->calculateActualFramesize();
    frame->framesize_global->setInitializer(
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), frame_size)
    );
    frame->framesize_global->setConstant(true);
  }
}

} // namespace tr

namespace absyn {

tr::ValAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return this->root_->Translate(venv, tenv, level, errormsg);
}

void TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv, tr::Level *level,
                        err::ErrorMsg *errormsg) const {
 /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::NameAndTy *> &type_dec_list = this->types_->GetList();
  for (auto &type_dec : type_dec_list) {
    tenv->Enter(type_dec->name_, new type::NameTy(type_dec->name_, nullptr));
  }

  for (auto &type_dec : type_dec_list) {
    type::NameTy *name_ty = static_cast<type::NameTy *>(tenv->Look(type_dec->name_));
    type::Ty *ty = type_dec->ty_->Translate(tenv, errormsg);
    name_ty->ty_ = ty;
    ty->GetLLVMType();
  }
}

void FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  llvm::BasicBlock &last_bb = ir_module->getFunction(level->frame_->GetLabel())->back();

  const std::list<absyn::FunDec *> &fun_dec_list = this->functions_->GetList();
  for (auto &fun_dec : fun_dec_list) {
    // build llvm::FunctionType and llvm::Function
    type::Ty *result_ty;
    if (fun_dec->result_) {
      result_ty = tenv->Look(fun_dec->result_)->ActualTy();
    } else {
      result_ty = type::VoidTy::Instance();
    }
    llvm::Type *result_type = result_ty->GetLLVMType();
    const std::list<absyn::Field *> &field_list = fun_dec->params_->GetList();
    std::vector<llvm::Type *> field_types(2, llvm::Type::getInt64Ty(ir_module->getContext()));
    for (auto &field : field_list) {
      llvm::Type *field_type = tenv->Look(field->typ_)->ActualTy()->GetLLVMType();
      field_types.push_back(field_type);
    }

    llvm::FunctionType *func_type = llvm::FunctionType::get(
      result_type, field_types, false
    );
    llvm::Function *func = llvm::Function::Create(
      func_type, llvm::GlobalValue::ExternalLinkage, fun_dec->name_->Name(), ir_module
    );
    functions.insert({fun_dec->name_->Name(), func});

    // build new level
    std::list<bool> formals;
    for (auto &field : field_list) {
      formals.push_back(field->escape_);
    }
    tr::Level *new_level = tr::Level::newLevel(level, fun_dec->name_, formals);
    frame_info.push_back({fun_dec->name_->Name(), new_level->frame_});

    // add to venv
    type::TyList *ty_list = fun_dec->params_->MakeFormalTyList(tenv, errormsg);
    env::FunEntry *entry = new env::FunEntry(new_level, ty_list, result_ty, func_type, func);
    venv->Enter(fun_dec->name_, entry);
  }

  for (auto &fun_dec : fun_dec_list) {
    venv->BeginScope();
    llvm::Function *func = functions[fun_dec->name_->Name()];
    env::FunEntry *entry = static_cast<env::FunEntry *>(venv->Look(fun_dec->name_));
    tr::Level *new_level = entry->level_;
    const std::list<absyn::Field *> &field_list = fun_dec->params_->GetList();

    // set basic block
    llvm::BasicBlock *func_block = llvm::BasicBlock::Create(
      ir_module->getContext(), 
      fun_dec->name_->Name(), 
      func
    );
    ir_builder->SetInsertPoint(func_block);

    // read the first argument and set stack pointer
    auto args = func->args();
    auto args_it = args.begin();
    llvm::Argument &first_arg = *args_it;
    llvm::Value *val = &first_arg;
    llvm::Value *frame_sz = ir_builder->CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), 
      new_level->frame_->framesize_global, 
      fun_dec->name_->Name() + "_local_framesize"
    );
    std::string sp_name = fun_dec->name_->Name() + "_sp";
    llvm::Value *sp = ir_builder->CreateSub(val, frame_sz, sp_name);
    new_level->set_sp(sp);
    
    // read the second argument and store static link
    std::list<frame::Access *> *formal_access_list = new_level->frame_->formals_;
    auto formal_access_it = formal_access_list->begin();
    frame::Access *access = *formal_access_it;
    llvm::Value *sl_addr = access->ToLLVMVal(new_level->get_sp());
    llvm::Value *sl_ptr = ir_builder->CreateIntToPtr(
      sl_addr, 
      llvm::PointerType::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0), 
      "sl_ptr"
    );
    llvm::Argument &second_arg = *(++args_it);
    llvm::Value *sl = &second_arg;
    ir_builder->CreateStore(sl, sl_ptr);

    // handle actual params
    auto field_it = field_list.begin();
    args_it++, formal_access_it++;
    // iterate three lists at a time
    for (; field_it != field_list.end(); args_it++, formal_access_it++, field_it++) {
      // add to venv
      frame::Access *access = *formal_access_it;
      tr::Access *tr_access = new tr::Access(new_level, access);
      venv->Enter(
        (*field_it)->name_, 
        new env::VarEntry(tr_access, tenv->Look((*field_it)->typ_))
      );

      // store on stack
      llvm::Value *addr = (*formal_access_it)->ToLLVMVal(new_level->get_sp());
      llvm::Argument &arg = *args_it;
      llvm::Value *val = &arg;
      llvm::PointerType *ptr_type = llvm::PointerType::get(val->getType(), 0);
      llvm::Value* ptr = ir_builder->CreateIntToPtr(addr, ptr_type);
      ir_builder->CreateStore(val, ptr);
    }

    // translate body
    tr::ValAndTy *body = fun_dec->body_->Translate(venv, tenv, new_level, errormsg);

    // return
    if (!fun_dec->result_) {
      ir_builder->CreateRetVoid();
    } else {
      if (body->val_->getType()->isIntegerTy(1)) {
        body->val_ = ir_builder->CreateZExt(
          body->val_, 
          llvm::Type::getInt32Ty(ir_module->getContext())
        );
      }
      ir_builder->CreateRet(body->val_);
    }

    ir_builder->SetInsertPoint(&last_bb);
    venv->EndScope();
  }
}

void VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv, tr::Level *level,
                       err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  
  tr::Access *access = tr::Access::AllocLocal(level, this->escape_);

  tr::ValAndTy *init = this->init_->Translate(venv, tenv, level, errormsg);
  type::Ty *ty = init->ty_;

  llvm::Value *val;
  llvm::PointerType *ptr_type;

  if (typeid(*ty) == typeid(type::IntTy)) {
    val = init->val_;
    ptr_type = llvm::PointerType::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0);
  } else {
    if (typeid(*ty) == typeid(type::NilTy)) {
      val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0);
    } else {
      val = ir_builder->CreatePtrToInt(
        init->val_, 
        llvm::Type::getInt64Ty(ir_module->getContext())
      );
    }
    ptr_type = llvm::PointerType::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0);
  }

  llvm::Value *var_addr = access->access_->ToLLVMVal(level->get_sp());
  llvm::Value *ptr = ir_builder->CreateIntToPtr(var_addr, ptr_type);
  ir_builder->CreateStore(val, ptr);
  venv->Enter(this->var_, new env::VarEntry(access, init->ty_->ActualTy(), false));
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  type::Ty *ty = tenv->Look(this->name_);
  return new type::NameTy(this->name_, ty);
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  type::FieldList *field_list = this->record_->MakeFieldList(tenv, errormsg);
  return new type::RecordTy(field_list);
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  type::Ty *ty = tenv->Look(this->array_);
  return new type::ArrayTy(ty);
}

tr::ValAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  env::VarEntry *var_entry = static_cast<env::VarEntry *>(venv->Look(this->sym_));

  type::Ty *ty = var_entry->ty_->ActualTy();
  tr::Access *access = var_entry->access_;
  frame::Access *in_frame_access = access->access_;
  tr::Level *curr_level = level;
  tr::Level *var_level = access->level_;

  llvm::Value *sp = level->get_sp();
  while (curr_level != var_level) {
    std::list<frame::Access *> *formal_access_list = curr_level->frame_->formals_;
    auto formal_access_it = formal_access_list->begin();
    frame::Access *access = *formal_access_it;
    llvm::Value *sl_addr = access->ToLLVMVal(sp);
    llvm::Value *sl_ptr = ir_builder->CreateIntToPtr(
      sl_addr, 
      llvm::PointerType::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0)
    );
    sp = ir_builder->CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), 
      sl_ptr
    );
    curr_level = curr_level->parent_;
  }

  llvm::Value *var_addr = in_frame_access->ToLLVMVal(sp);
  llvm::PointerType *ptr_type = llvm::PointerType::get(ty->GetLLVMType(), 0);
  std::string ptr_name = var_level->frame_->GetLabel() + "_" + this->sym_->Name() + "_ptr";
  llvm::Value *ptr = ir_builder->CreateIntToPtr(var_addr, ptr_type, ptr_name);

  // note: returned value is the pointer of the variable
  return new tr::ValAndTy(ptr, ty, ir_builder->GetInsertBlock());
}

tr::ValAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = this->var_->Translate(venv, tenv, level, errormsg);
  llvm::Value *struct_ptr = ir_builder->CreateLoad(var->ty_->GetLLVMType(), var->val_);
  type::RecordTy *record_ty = static_cast<type::RecordTy *>(var->ty_);
  int idx = record_ty->field_index[this->sym_->Name()];

  llvm::Value *field_ptr = ir_builder->CreateGEP(
    struct_ptr->getType()->getPointerElementType(),
    struct_ptr,
    {
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), 
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), idx)
    }
  );

  type::Ty *field_ty;
  const std::list<type::Field *> &field_list = record_ty->fields_->GetList();
  int i = 0;
  for (auto &field : field_list) {
    if (i == idx) {
      field_ty = field->ty_->ActualTy();
      break;
    }
    i++;
  }

  return new tr::ValAndTy(field_ptr, field_ty, ir_builder->GetInsertBlock());
}

tr::ValAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level,
                                      err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = this->var_->Translate(venv, tenv, level, errormsg);
  llvm::Value *array_ptr = ir_builder->CreateLoad(var->ty_->GetLLVMType(), var->val_);
  tr::ValAndTy *subscript = this->subscript_->Translate(venv, tenv, level, errormsg);
  llvm::Value *element_ptr = ir_builder->CreateGEP(
    var->ty_->GetLLVMType()->getPointerElementType(), 
    array_ptr, 
    subscript->val_
  );
  return new tr::ValAndTy(
    element_ptr, 
    static_cast<type::ArrayTy *>(var->ty_)->ty_->ActualTy(), 
    ir_builder->GetInsertBlock()
  );
}

tr::ValAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = this->var_->Translate(venv, tenv, level, errormsg);
  llvm::Value *val = ir_builder->CreateLoad(var->ty_->GetLLVMType(), var->val_);
  return new tr::ValAndTy(val, var->ty_, ir_builder->GetInsertBlock());
}

tr::ValAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return new tr::ValAndTy(
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), 
    type::NilTy::Instance(), 
    ir_builder->GetInsertBlock()
  );
}

tr::ValAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return new tr::ValAndTy(
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), this->val_),
    type::IntTy::Instance(), 
    ir_builder->GetInsertBlock()
  );
}

tr::ValAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return new tr::ValAndTy(
    type::StringTy::CreateGlobalStringStructPtr(this->str_), 
    type::StringTy::Instance(), 
    ir_builder->GetInsertBlock()
  );
}

tr::ValAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::Exp *> &arg_list = this->args_->GetList();

  env::FunEntry *entry = static_cast<env::FunEntry *>(venv->Look(this->func_));
  llvm::Function *func = entry->func_;
  
  std::vector<llvm::Value*> vec;
  if (functions.find(this->func_->Name()) != functions.end()) {
    // means the function is declared by us, need 2 more arguments
    llvm::Value *sp = level->get_sp();
    vec.push_back(sp);  // first argument is the sp of current level
    // second argument is the sp of callee's father level
    // we have to trace back
    tr::Level *curr_level = level;
    tr::Level *func_parent_level = entry->level_->parent_;
    while (curr_level != func_parent_level) {
      std::list<frame::Access *> *formal_access_list = curr_level->frame_->formals_;
      auto formal_access_it = formal_access_list->begin();
      frame::Access *access = *formal_access_it;
      llvm::Value *sl_addr = access->ToLLVMVal(sp);
      llvm::Value *sl_ptr = ir_builder->CreateIntToPtr(
        sl_addr, 
        llvm::PointerType::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0)
      );
      sp = ir_builder->CreateLoad(
        llvm::Type::getInt64Ty(ir_module->getContext()), 
        sl_ptr
      );
      curr_level = curr_level->parent_;
    }
    vec.push_back(sp);
  }

  for (auto &exp : arg_list) {
    tr::ValAndTy *arg = exp->Translate(venv, tenv, level, errormsg);
    if (llvm::isa<llvm::LoadInst>(arg->val_))
      arg->val_->setName("actual_parm");
    vec.push_back(arg->val_);
  }

  level->frame_->AllocOutgoSpace(vec.size() * reg_manager->WordSize());

  llvm::Value *result = ir_builder->CreateCall(func, llvm::ArrayRef<llvm::Value *>(vec));
  return new tr::ValAndTy(result, entry->result_, ir_builder->GetInsertBlock());
}

tr::ValAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  if (this->oper_ != Oper::AND_OP && this->oper_ != Oper::OR_OP) {
    tr::ValAndTy *left = this->left_->Translate(venv, tenv, level, errormsg);
    tr::ValAndTy *right = this->right_->Translate(venv, tenv, level, errormsg);

    llvm::Value *val = nullptr;

    switch (this->oper_) {
    case Oper::PLUS_OP:
      val = ir_builder->CreateAdd(left->val_, right->val_);
      break;
    case Oper::MINUS_OP:
      val = ir_builder->CreateSub(left->val_, right->val_);
      break;
    case Oper::TIMES_OP:
      val = ir_builder->CreateMul(left->val_, right->val_);
      break;
    case Oper::DIVIDE_OP:
      val = ir_builder->CreateSDiv(left->val_, right->val_);
      break;
    case Oper::GE_OP:
      val = ir_builder->CreateICmpSGE(left->val_, right->val_);
      break;
    case Oper::GT_OP:
      val = ir_builder->CreateICmpSGT(left->val_, right->val_);
      break;
    case Oper::LE_OP:
      val = ir_builder->CreateICmpSLE(left->val_, right->val_);
      break;
    case Oper::LT_OP:
      val = ir_builder->CreateICmpSLT(left->val_, right->val_);
      break;
    case Oper::EQ_OP:
      if (typeid(*left->ty_) == typeid(type::StringTy) && 
          typeid(*right->ty_) == typeid(type::StringTy)) {
        val = ir_builder->CreateCall(string_equal, {left->val_, right->val_});
      } else {
        if (typeid(*left->ty_) == typeid(type::NilTy) || 
            typeid(*right->ty_) == typeid(type::NilTy)) {
          llvm::PointerType *type;
          if (typeid(*left->ty_) == typeid(type::NilTy)) {
            type = static_cast<llvm::PointerType *>(right->ty_->ActualTy()->GetLLVMType());
            left->val_ = llvm::ConstantPointerNull::get(type);
          } else {
            type = static_cast<llvm::PointerType *>(left->ty_->ActualTy()->GetLLVMType());
            right->val_ = llvm::ConstantPointerNull::get(type);
          }
        }

        val = ir_builder->CreateICmpEQ(left->val_, right->val_);
      }
      break;
    case Oper::NEQ_OP:
      if (typeid(*left->ty_) == typeid(type::StringTy) && 
          typeid(*right->ty_) == typeid(type::StringTy)) {
        val = ir_builder->CreateCall(string_equal, {left->val_, right->val_});
        val = ir_builder->CreateNot(val);
      } else {
        if (typeid(*left->ty_) == typeid(type::NilTy) || 
            typeid(*right->ty_) == typeid(type::NilTy)) {
          llvm::PointerType *type;
          if (typeid(*left->ty_) == typeid(type::NilTy)) {
            type = static_cast<llvm::PointerType *>(right->ty_->ActualTy()->GetLLVMType());
            left->val_ = llvm::ConstantPointerNull::get(type);
          } else {
            type = static_cast<llvm::PointerType *>(left->ty_->ActualTy()->GetLLVMType());
            right->val_ = llvm::ConstantPointerNull::get(type);
          }
        }
        
        val = ir_builder->CreateICmpNE(left->val_, right->val_);
      }
      break;
    }

    return new tr::ValAndTy(val, type::IntTy::Instance(), ir_builder->GetInsertBlock());
  } else {
    std::string func_name = level->frame_->GetLabel();
    llvm::Function *func = functions.at(func_name);

    if (this->oper_ == Oper::AND_OP) {
      llvm::BasicBlock *opand_right_test = 
        llvm::BasicBlock::Create(ir_module->getContext(), "opand_right_test", func);
      llvm::BasicBlock *opand_next = 
        llvm::BasicBlock::Create(ir_module->getContext(), "opand_next", func);

      tr::ValAndTy *left = this->left_->Translate(venv, tenv, level, errormsg);
      ir_builder->CreateCondBr(left->val_, opand_right_test, opand_next);

      ir_builder->SetInsertPoint(opand_right_test);
      tr::ValAndTy *right = this->right_->Translate(venv, tenv, level, errormsg);
      ir_builder->CreateBr(opand_next);

      ir_builder->SetInsertPoint(opand_next);
      llvm::PHINode *phi = ir_builder->CreatePHI(llvm::Type::getInt1Ty(ir_module->getContext()), 2);
      phi->addIncoming(llvm::ConstantInt::getFalse(ir_module->getContext()), left->last_bb_);
      phi->addIncoming(right->val_, right->last_bb_);
      llvm::Value *val = phi;
      return new tr::ValAndTy(val, type::IntTy::Instance(), opand_next);
    } else if (this->oper_ == Oper::OR_OP) {
      llvm::BasicBlock *opor_right_test = 
        llvm::BasicBlock::Create(ir_module->getContext(), "opor_right_test", func);
      llvm::BasicBlock *opor_next = 
        llvm::BasicBlock::Create(ir_module->getContext(), "opor_next", func);

      tr::ValAndTy *left = this->left_->Translate(venv, tenv, level, errormsg);
      ir_builder->CreateCondBr(left->val_, opor_next, opor_right_test);

      ir_builder->SetInsertPoint(opor_right_test);
      tr::ValAndTy *right = this->right_->Translate(venv, tenv, level, errormsg);
      ir_builder->CreateBr(opor_next);

      ir_builder->SetInsertPoint(opor_next);
      llvm::PHINode *phi = ir_builder->CreatePHI(llvm::Type::getInt1Ty(ir_module->getContext()), 2);
      phi->addIncoming(llvm::ConstantInt::getTrue(ir_module->getContext()), left->last_bb_);
      phi->addIncoming(right->val_, right->last_bb_);
      llvm::Value *val = phi;
      return new tr::ValAndTy(val, type::IntTy::Instance(), opor_next);
    }
  }

  assert(0);
  return nullptr;
}

tr::ValAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  type::Ty *ty = tenv->Look(this->typ_)->ActualTy();
  llvm::Value *result = ir_builder->CreateCall(
    alloc_record, 
    llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_module->getContext()), 
      static_cast<type::RecordTy *>(ty)->fields_->GetList().size() * reg_manager->WordSize()
    )
  );
  llvm::Value *struct_ptr = ir_builder->CreateIntToPtr(result, ty->GetLLVMType());

  const std::list<absyn::EField *> &efield_list = this->fields_->GetList();
  int idx = 0;
  for (auto &efield : efield_list) {
    tr::ValAndTy *exp = efield->exp_->Translate(venv, tenv, level, errormsg);
    llvm::Value *field_ptr = ir_builder->CreateGEP(
      struct_ptr->getType()->getPointerElementType(), 
      struct_ptr, 
      {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), 
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), idx++)
      }
    );
    ir_builder->CreateStore(exp->val_, field_ptr);
  }

  // note: returned value is the pointer of the struct
  return new tr::ValAndTy(struct_ptr, ty, ir_builder->GetInsertBlock());
}

tr::ValAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::Exp *> &exp_list = this->seq_->GetList();
  tr::ValAndTy *result = nullptr;
  for (auto &exp : exp_list) {
    result = exp->Translate(venv, tenv, level, errormsg);
  }
  return result;
}

tr::ValAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = this->var_->Translate(venv, tenv, level, errormsg);
  tr::ValAndTy *exp = this->exp_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateStore(exp->val_, var->val_);
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance(), ir_builder->GetInsertBlock());
}

tr::ValAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  std::string func_name = level->frame_->GetLabel();
  llvm::Function *func = functions.at(func_name);
  llvm::BasicBlock *if_test = 
    llvm::BasicBlock::Create(ir_module->getContext(), "if_test", func);
  llvm::BasicBlock *if_then = 
    llvm::BasicBlock::Create(ir_module->getContext(), "if_then", func);
  llvm::BasicBlock *if_else = nullptr;
  if (this->elsee_) {
    if_else = llvm::BasicBlock::Create(ir_module->getContext(), "if_else", func);
  }
  llvm::BasicBlock *if_next = 
    llvm::BasicBlock::Create(ir_module->getContext(), "if_next", func);
  
  ir_builder->CreateBr(if_test);

  ir_builder->SetInsertPoint(if_test);
  tr::ValAndTy *test = this->test_->Translate(venv, tenv, level, errormsg);
  if (!test->val_->getType()->isIntegerTy(1)) {
    test->val_ = ir_builder->CreateICmpNE(
      test->val_, 
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0)
    );
  }
  if (if_else) {
    ir_builder->CreateCondBr(test->val_, if_then, if_else);
  } else {
    ir_builder->CreateCondBr(test->val_, if_then, if_next);
  }

  ir_builder->SetInsertPoint(if_then);
  tr::ValAndTy *then = this->then_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateBr(if_next);

  tr::ValAndTy *elsee = nullptr;
  if (if_else) {
    ir_builder->SetInsertPoint(if_else);
    elsee = this->elsee_->Translate(venv, tenv, level, errormsg);
    ir_builder->CreateBr(if_next);
  }

  ir_builder->SetInsertPoint(if_next);

  if (elsee && typeid(*then->ty_->ActualTy()) != typeid(type::VoidTy) && 
      typeid(*elsee->ty_->ActualTy()) != typeid(type::VoidTy)) {
    type::Ty *ty = nullptr;
    llvm::Type *phi_type;
    if (typeid(*then->ty_->ActualTy()) != typeid(type::NilTy)) {
      ty = then->ty_->ActualTy();
      phi_type = ty->GetLLVMType();
    } else if (typeid(*elsee->ty_->ActualTy()) != typeid(type::NilTy)) {
      ty = elsee->ty_->ActualTy();
      phi_type = ty->GetLLVMType();
    }

    if (!ty) {
      ty = type::NilTy::Instance();
      phi_type = llvm::PointerType::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0);
    }
    
    llvm::PHINode *phi = ir_builder->CreatePHI(phi_type, 2);
    if (typeid(*then->ty_->ActualTy()) == typeid(type::NilTy)) {
      then->val_ = llvm::ConstantPointerNull::get(static_cast<llvm::PointerType *>(phi_type));
    }
    if (typeid(*elsee->ty_->ActualTy()) == typeid(type::NilTy)) {
      elsee->val_ = llvm::ConstantPointerNull::get(static_cast<llvm::PointerType *>(phi_type));
    }

    phi->addIncoming(then->val_, then->last_bb_);
    phi->addIncoming(elsee->val_, elsee->last_bb_);
    llvm::Value *val = phi;
    return new tr::ValAndTy(val, ty, if_next);
  } else {
    return new tr::ValAndTy(nullptr, type::VoidTy::Instance(), if_next);
  }
}

tr::ValAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  std::string func_name = level->frame_->GetLabel();
  llvm::Function *func = functions.at(func_name);
  llvm::BasicBlock *while_test = 
    llvm::BasicBlock::Create(ir_module->getContext(), "while_test", func);
  llvm::BasicBlock *while_body = 
    llvm::BasicBlock::Create(ir_module->getContext(), "while_body", func);
  llvm::BasicBlock *while_next = 
    llvm::BasicBlock::Create(ir_module->getContext(), "while_next", func);

  loop_stack.push(while_next);

  ir_builder->CreateBr(while_test);

  ir_builder->SetInsertPoint(while_test);
  tr::ValAndTy *test = this->test_->Translate(venv, tenv, level, errormsg);
  if (!test->val_->getType()->isIntegerTy(1)) {
    test->val_ = ir_builder->CreateICmpNE(
      test->val_, 
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0)
    );
  }
  ir_builder->CreateCondBr(test->val_, while_body, while_next);

  ir_builder->SetInsertPoint(while_body);
  tr::ValAndTy *body = this->body_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateBr(while_test);

  ir_builder->SetInsertPoint(while_next);

  loop_stack.pop();

  return new tr::ValAndTy(nullptr, type::VoidTy::Instance(), while_next);
}

tr::ValAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  std::string func_name = level->frame_->GetLabel();
  llvm::Function *func = functions.at(func_name);

  // FIX: for_test block
  llvm::BasicBlock *for_incre = 
    llvm::BasicBlock::Create(ir_module->getContext(), "for_incre", func);
  llvm::BasicBlock *for_body = 
    llvm::BasicBlock::Create(ir_module->getContext(), "for_body", func);
  llvm::BasicBlock *for_next = 
    llvm::BasicBlock::Create(ir_module->getContext(), "for_next", func);

  loop_stack.push(for_next);

  tr::ValAndTy *lo = this->lo_->Translate(venv, tenv, level, errormsg);
  tr::ValAndTy *hi = this->hi_->Translate(venv, tenv, level, errormsg);

  venv->BeginScope();
  tr::Access *access = tr::Access::AllocLocal(level, this->escape_);

  llvm::Value *i_addr = access->access_->ToLLVMVal(level->get_sp());
  llvm::PointerType *ptr_type = 
    llvm::PointerType::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0);
  llvm::Value *i_ptr = ir_builder->CreateIntToPtr(i_addr, ptr_type);

  ir_builder->CreateStore(lo->val_, i_ptr);
  venv->Enter(this->var_, new env::VarEntry(access, lo->ty_->ActualTy(), false));

  ir_builder->CreateBr(for_body);

  ir_builder->SetInsertPoint(for_incre);
  llvm::Value *i_val = ir_builder->CreateLoad(
    llvm::Type::getInt32Ty(ir_module->getContext()), i_ptr
  );
  llvm::Value *i_new_val = ir_builder->CreateAdd(
    i_val, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 1)
  );
  ir_builder->CreateStore(i_new_val, i_ptr);
  llvm::Value *cond = ir_builder->CreateICmpSLE(i_new_val, hi->val_);
  ir_builder->CreateCondBr(cond, for_body, for_next);

  ir_builder->SetInsertPoint(for_body);
  this->body_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateBr(for_incre);

  ir_builder->SetInsertPoint(for_next);
  venv->EndScope();
  loop_stack.pop();

  return new tr::ValAndTy(nullptr, type::VoidTy::Instance(), for_next);
}

tr::ValAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  ir_builder->CreateBr(loop_stack.top());
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance(), ir_builder->GetInsertBlock());
}

tr::ValAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  venv->BeginScope();
  tenv->BeginScope();

  const std::list<absyn::Dec *> &dec_list = this->decs_->GetList();
  for (const auto &dec : dec_list)
    dec->Translate(venv, tenv, level, errormsg);
  
  tr::ValAndTy *body = this->body_->Translate(venv, tenv, level, errormsg);

  tenv->EndScope();
  venv->EndScope();

  return body;
}

tr::ValAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *size = this->size_->Translate(venv, tenv, level, errormsg);
  tr::ValAndTy *init = this->init_->Translate(venv, tenv, level, errormsg);
  // FIX: convert other types to int64
  llvm::ConstantInt *constant_value = llvm::dyn_cast<llvm::ConstantInt>(init->val_);
  llvm::Value *init_val_int64;
  if (constant_value) {
    llvm::APInt extended_value = constant_value->getValue().sext(64);
    init_val_int64 = llvm::ConstantInt::get(
        llvm::Type::getInt64Ty(ir_module->getContext()),
        extended_value
    );
  }
  llvm::Value *result = ir_builder->CreateCall(init_array, {size->val_, init_val_int64});
  type::ArrayTy *array_ty = static_cast<type::ArrayTy *>(tenv->Look(this->typ_)->ActualTy());
  llvm::Value *ptr = ir_builder->CreateIntToPtr(
    result, 
    llvm::PointerType::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0)
  );

  // note: returned value is the pointer of the array
  return new tr::ValAndTy(ptr, array_ty, ir_builder->GetInsertBlock());
}

tr::ValAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance(), ir_builder->GetInsertBlock());
}

} // namespace absyn
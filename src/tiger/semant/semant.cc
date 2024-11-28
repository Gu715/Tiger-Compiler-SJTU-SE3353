#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include <unordered_set>

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  this->root_->SemAnalyze(venv, tenv, 0, errormsg);
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  env::EnvEntry *entry = venv->Look(this->sym_);
  if (entry && typeid(*entry) == typeid(env::VarEntry)) {
    return (static_cast<env::VarEntry *>(entry))->ty_->ActualTy();
  } else {
    errormsg->Error(this->pos_, "undefined variable %s", this->sym_->Name().data());
  }
  return type::IntTy::Instance();
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *var_ty = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*var_ty) != typeid(type::RecordTy)) {
    errormsg->Error(this->var_->pos_, "not a record type");
    return type::IntTy::Instance();
  }

  std::list<type::Field *> &fieldList = (static_cast<type::RecordTy *>(var_ty))->fields_->GetList();
  for (type::Field *field : fieldList) {
    if (field->name_ == this->sym_) {
      return field->ty_->ActualTy();
    }
  } 
  errormsg->Error(this->pos_, "field %s doesn't exist", this->sym_->Name().data());
  return type::IntTy::Instance();
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *var_ty = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*var_ty) != typeid(type::ArrayTy)) {
    errormsg->Error(this->var_->pos_, "array type required");
    return type::IntTy::Instance();
  }

  type::Ty *exp_ty = this->subscript_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*exp_ty) != typeid(type::IntTy)) {
    errormsg->Error(this->pos_, "integer required");
    return type::IntTy::Instance();
  }
  return (static_cast<type::ArrayTy *>(var_ty))->ty_->ActualTy();
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  env::EnvEntry *entry = venv->Look(this->func_);
  if (!entry || typeid(*entry) != typeid(env::FunEntry)) {
    errormsg->Error(this->pos_, "undefined function %s", this->func_->Name().data());
    return type::IntTy::Instance();
  }

  const std::list<type::Ty *> &formal_list = (static_cast<env::FunEntry *>(entry))->formals_->GetList();
  const std::list<Exp *> &exp_list = this->args_->GetList();

  auto formal_it = formal_list.begin();
  auto exp_it = exp_list.begin();
  for (; formal_it != formal_list.end() && exp_it != exp_list.end(); formal_it++, exp_it++) {
    type::Ty *exp_ty = (*exp_it)->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (!exp_ty->IsSameType(*formal_it)) {
      errormsg->Error((*exp_it)->pos_, "para type mismatch");
    }
  }

  if (formal_it != formal_list.end())
    errormsg->Error(this->pos_, "too few params in function %s", this->func_->Name().data());
  if (exp_it != exp_list.end())
    errormsg->Error((*exp_it)->pos_, "too many params in function %s", this->func_->Name().data());

  return (static_cast<env::FunEntry *>(entry))->result_->ActualTy();
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *left_ty = left_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *right_ty = right_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (oper_ == absyn::PLUS_OP || oper_ == absyn::MINUS_OP || 
    oper_ == absyn::TIMES_OP || oper_ == absyn::DIVIDE_OP ||
    oper_ == absyn::AND_OP || oper_ == absyn::OR_OP) {
    if (typeid(*left_ty) != typeid(type::IntTy))
      errormsg->Error(left_->pos_, "integer required");
    if (typeid(*right_ty) != typeid(type::IntTy))
      errormsg->Error(right_->pos_, "integer required");
    return type::IntTy::Instance();
  } else if (oper_ == absyn::LT_OP || oper_ == absyn::LE_OP ||
    oper_ == absyn::GT_OP || oper_ == absyn::GE_OP) {
    if (!left_ty->IsSameType(right_ty)) {
      errormsg->Error(this->pos_, "same type required");
      return type::IntTy::Instance();
    }
    if (typeid(*left_ty) != typeid(type::IntTy) && typeid(*left_ty) != typeid(type::StringTy))
      errormsg->Error(left_->pos_, "integer or string required");
    if (typeid(*right_ty) != typeid(type::IntTy) && typeid(*right_ty) != typeid(type::StringTy))
      errormsg->Error(right_->pos_, "integer or string required");
    return type::IntTy::Instance();
  } else {
    if (typeid(*left_ty) == typeid(type::NilTy)) {
      if (typeid(*right_ty) != typeid(type::RecordTy))
        errormsg->Error(right_->pos_, "record required");
      return type::IntTy::Instance();
    }
    if (typeid(*right_ty) == typeid(type::NilTy)) {
      if (typeid(*left_ty) != typeid(type::RecordTy))
        errormsg->Error(left_->pos_, "record required");
      return type::IntTy::Instance();
    }
    if (!left_ty->IsSameType(right_ty)) {
      errormsg->Error(this->pos_, "same type required");
      return type::IntTy::Instance();
    }
    if (typeid(*left_ty) == typeid(type::VoidTy))
      errormsg->Error(left_->pos_, "non-void expression required");
    if (typeid(*right_ty) == typeid(type::VoidTy))
      errormsg->Error(right_->pos_, "non-void expression required");
    return type::IntTy::Instance();
  }
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *ty = tenv->Look(this->typ_);
  if (!ty) {
    errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
    return type::IntTy::Instance();
  }

  ty = ty->ActualTy();
  if (typeid(*ty) != typeid(type::RecordTy)) {
    errormsg->Error(this->pos_, "not a record type");
    return type::IntTy::Instance();
  }

  const std::list<absyn::EField *> &field_list = this->fields_->GetList();
  const std::list<type::Field *> &expect_field_list = (static_cast<type::RecordTy *>(ty))->fields_->GetList();

  auto field_it = field_list.begin();
  auto expect_field_it = expect_field_list.begin();
  for(; field_it != field_list.end() && expect_field_it != expect_field_list.end(); field_it++, expect_field_it++) {
    std::string field_name = (*field_it)->name_->Name(), expect_field_name = (*expect_field_it)->name_->Name();
    if (field_name != expect_field_name) {
      errormsg->Error((*field_it)->exp_->pos_, "field name mismatch");
      continue;
    }
    type::Ty *exp_ty = (*field_it)->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
    type::Ty *expect_ty = (*expect_field_it)->ty_->ActualTy();
    if (!exp_ty->IsSameType(expect_ty) &&
        !(typeid(*expect_ty) == typeid(type::RecordTy) && typeid(*exp_ty) == typeid(type::NilTy)))
      errormsg->Error((*field_it)->exp_->pos_, "field type mismatch");
  }

  if (field_it != field_list.end())
    errormsg->Error(this->pos_, "too many fields in record %s", this->typ_->Name().data());
  if (expect_field_it != expect_field_list.end())
    errormsg->Error(this->pos_, "too few fields in record %s", this->typ_->Name().data());
  return ty;
}

type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  const std::list<absyn::Exp *> &exp_list = this->seq_->GetList();
  type::Ty *ty = nullptr;
  for (absyn::Exp *exp : exp_list)
    ty = exp->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!ty)
    return type::VoidTy::Instance();
  return ty;
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *var_ty = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *exp_ty = this->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (typeid(*var_) == typeid(absyn::SimpleVar)) {
    env::EnvEntry *entry = venv->Look((static_cast<absyn::SimpleVar *>(this->var_))->sym_);
    if (entry && entry->readonly_) {
      errormsg->Error(this->pos_, "loop variable can't be assigned");
      return type::VoidTy::Instance();
    }
  }

  if (!var_ty->IsSameType(exp_ty)) {
    errormsg->Error(this->exp_->pos_, "unmatched assign exp");
  }
  return type::VoidTy::Instance();
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *test_ty = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*test_ty) != typeid(type::IntTy)) {
    errormsg->Error(this->test_->pos_, "integer required");
    return type::IntTy::Instance();
  }

  type::Ty *then_ty = this->then_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *else_ty = nullptr;
  if (!this->elsee_) {
    if (typeid(*then_ty) != typeid(type::VoidTy))
      errormsg->Error(this->then_->pos_, "if-then exp's body must produce no value");
    return type::VoidTy::Instance();
  } else {
    else_ty = this->elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (!then_ty->IsSameType(else_ty)) {
      errormsg->Error(this->pos_, "then exp and else exp type mismatch");
      return type::VoidTy::Instance();
    }
    return then_ty;
  }
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *test_ty = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*test_ty) != typeid(type::IntTy))
    errormsg->Error(this->test_->pos_, "integer required");

  type::Ty *body_ty = this->body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  if (typeid(*body_ty) != typeid(type::VoidTy))
    errormsg->Error(this->body_->pos_, "while body must produce no value");
  return type::VoidTy::Instance();
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *lo_ty = this->lo_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *hi_ty = this->hi_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*lo_ty) != typeid(type::IntTy))
    errormsg->Error(this->lo_->pos_, "for exp's range type is not integer");
  if (typeid(*hi_ty) != typeid(type::IntTy))
    errormsg->Error(this->hi_->pos_, "for exp's range type is not integer");

  venv->BeginScope();
  venv->Enter(this->var_, new env::VarEntry(type::IntTy::Instance(), true));

  type::Ty *body_ty = this->body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  if (typeid(*body_ty) != typeid(type::VoidTy))
    errormsg->Error(this->body_->pos_, "for body must produce no value");
  
  venv->EndScope();
  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  if (labelcount <= 0)
    errormsg->Error(this->pos_, "break is not inside any loop");
  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  venv->BeginScope();
  tenv->BeginScope();
  for (absyn::Dec *dec : this->decs_->GetList())
      dec->SemAnalyze(venv, tenv, labelcount, errormsg);

  type::Ty *result = type::VoidTy::Instance();
  if (this->body_)
      result = body_->SemAnalyze(venv, tenv, labelcount, errormsg);
  
  tenv->EndScope();
  venv->EndScope();
  return result;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *ty = tenv->Look(this->typ_);
  if (!ty) {
    errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
    return type::IntTy::Instance();
  }

  ty = ty->ActualTy();
  if (typeid(*ty) != typeid(type::ArrayTy)) {
    errormsg->Error(this->pos_, "not an array type");
    return type::IntTy::Instance();
  }

  type::Ty *size_ty = this->size_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*size_ty) != typeid(type::IntTy))
    errormsg->Error(this->size_->pos_, "integer required");
  
  type::Ty *init_ty = this->init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *array_ty = (static_cast<type::ArrayTy *>(ty))->ty_;
  if (!init_ty->IsSameType(array_ty))
    errormsg->Error(this->init_->pos_, "type mismatch");
  
  return ty;
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  const std::list<absyn::FunDec *> &fun_dec_list = this->functions_->GetList();
  std::unordered_set<sym::Symbol *> funcs;
  for (absyn::FunDec *fun_dec : fun_dec_list) {
    if (funcs.find(fun_dec->name_) != funcs.end()) {
      errormsg->Error(fun_dec->pos_, "two functions have the same name");
      continue;
    }
    funcs.insert(fun_dec->name_);

    absyn::FieldList *params = fun_dec->params_;
    type::Ty *result_ty = type::VoidTy::Instance();
    if (fun_dec->result_) {
      type::Ty *ty = tenv->Look(fun_dec->result_);
      if (!ty) {
        errormsg->Error(fun_dec->pos_, "undefined type %s", fun_dec->result_->Name().data());
        continue;
      }
      result_ty = ty->ActualTy();
    }
    type::TyList *formals = params->MakeFormalTyList(tenv, errormsg);
    venv->Enter(fun_dec->name_, new env::FunEntry(formals, result_ty));
  }

  for (absyn::FunDec *fun_dec : fun_dec_list) {
    venv->BeginScope();
    absyn::FieldList *params = fun_dec->params_;
    type::TyList *formals = (static_cast<env::FunEntry *>(venv->Look(fun_dec->name_)))->formals_;
    auto formal_it = formals->GetList().begin();
    auto param_it = params->GetList().begin();
    for (; param_it != params->GetList().end() && formal_it != formals->GetList().end(); formal_it++, param_it++)
      venv->Enter((*param_it)->name_, new env::VarEntry(*formal_it));
    
    type::Ty *ty = fun_dec->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
    type::Ty *expect_ty = (static_cast<env::FunEntry *>(venv->Look(fun_dec->name_)))->result_;
    if (!ty->IsSameType(expect_ty)) {
      if (typeid(*expect_ty) == typeid(type::VoidTy))
        errormsg->Error(fun_dec->pos_, "procedure returns value");
      else
        errormsg->Error(fun_dec->pos_, "return type mismatch");
    }

    venv->EndScope();
  }
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *init_ty = this->init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!this->typ_) {
    if (typeid(*init_ty) == typeid(type::NilTy))
      errormsg->Error(this->pos_, "init should not be nil without type specified");
    venv->Enter(this->var_, new env::VarEntry(init_ty));
  } else {
    type::Ty *type = tenv->Look(this->typ_);
    if (!type) {
      errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
    } else {
      type = type->ActualTy();
      if ((typeid(*init_ty) != typeid(type::NilTy) && !init_ty->IsSameType(type))
       || (typeid(*init_ty) == typeid(type::NilTy) && typeid(*type) != typeid(type::RecordTy)))
        errormsg->Error(this->init_->pos_, "type mismatch");
      venv->Enter(this->var_, new env::VarEntry(type));
    }
  }
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  const std::list<absyn::NameAndTy *> &type_dec_list = this->types_->GetList();
  std::unordered_set<sym::Symbol *> types;
  for (absyn::NameAndTy *type_dec : type_dec_list) {
    if (types.find(type_dec->name_) != types.end()) {
      errormsg->Error(this->pos_, "two types have the same name");
      continue;
    }
    types.insert(type_dec->name_);
    tenv->Enter(type_dec->name_, new type::NameTy(type_dec->name_, nullptr));
  }

  for (absyn::NameAndTy *type_dec : type_dec_list) {
    type::Ty *ty = type_dec->ty_->SemAnalyze(tenv, errormsg);
    (static_cast<type::NameTy *>(tenv->Look(type_dec->name_)))->ty_ = ty;
  }

  bool has_cycle = false;
  for (absyn::NameAndTy *type_dec : type_dec_list) {
    type::Ty *ty = tenv->Look(type_dec->name_);

    if (ty && typeid(*ty) == typeid(type::NameTy)) {
      std::unordered_set<sym::Symbol *> syms;
      syms.insert(type_dec->name_);

      while (ty && typeid(*ty) == typeid(type::NameTy)) {
        type::NameTy *name_ty = static_cast<type::NameTy *>(ty);
        sym::Symbol *sym = name_ty->sym_;

        if (syms.find(sym) != syms.end()) {
          errormsg->Error(this->pos_, "illegal type cycle");
          has_cycle = true;
          break;
        }

        syms.insert(sym);
        ty = name_ty->ty_;
      }
    }

    if (has_cycle)
      break;
  }
}

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *ty = tenv->Look(this->name_);
  if (!ty) {
    errormsg->Error(this->pos_, "undefined type %s", this->name_->Name().data());
    return nullptr;
  }
  return new type::NameTy(this->name_, ty);
}

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::FieldList *field_list = this->record_->MakeFieldList(tenv, errormsg);
  return new type::RecordTy(field_list);
}

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *ty = tenv->Look(this->array_);
  if (!ty) {
    errormsg->Error(this->pos_, "undefined type %s", this->array_->Name().data());
    return nullptr;
  }
  return new type::ArrayTy(ty);
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());
}
} // namespace sem
